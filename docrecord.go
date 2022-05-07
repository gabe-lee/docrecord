package docrecord

import (
	"time"
	"unicode/utf8"

	structs "github.com/gabe-lee/genstructs"
)

type IDX = uint32 // Index type, max file size = 4 GB
type BUF uint8    // Buffer index
type OP uint8     // Operation Type
type FLAGS uint8  // Document flags

const (
	NeedsRedraw FLAGS = 1 << iota
	NeedsSave

	BufferExtensionSize     = 1024
	PieceListExtensionCount = 200
	HistoryExtensionCount   = 100
	OpCombineTimer          = time.Second * 5
	OpCombineMaxChars       = 50

	InitBuf BUF = 0
	AddBuf  BUF = 1
	NoBuf   BUF = 2

	Left  = structs.Left
	Right = structs.Right

	NoOp      OP = 0
	Write     OP = 1
	Delete    OP = 2
	Redaction OP = 3
)

type IdxTree = structs.BSTree[IDX]
type IdxNode = structs.BSNode[IDX]
type NodeSide = structs.NodeSide

type Operation struct {
	Type   OP
	Before OpSnap
	After  OpSnap
}

type OpSnap []PieceSnap

type PreOpSnapshot = map[*IdxNode]PieceSnap

type PieceSnap struct {
	Index       IDX
	Start       IDX
	End         IDX
	Chars       IDX
	CharsOnLeft IDX
	Parent      IDX
	Children    [2]IDX
}

type Piece struct {
	Buffer              BUF
	Start               IDX
	End                 IDX
	Chars               IDX
	CharsOnLeftChildren IDX
}

type Caret struct {
	NodeCharStart IDX
	CharIdx       IDX
	NodeCharEnd   IDX
	SelectionLen  IDX
	Node          *IdxNode
}

func (p Piece) Len() IDX {
	return p.End - p.Start
}

func (p Piece) CharsPlusCharsBelowOnLeft() IDX {
	return p.CharsOnLeftChildren + p.Chars
}

type History struct {
	Ops                      []Operation
	ActiveIndex              IDX
	LastOpPieceIdx           IDX
	LastOpType               OP
	LastOpTime               time.Time
	LastOpChars              IDX
	LastDeleteEndIdx         IDX
	CaretMoveSinceLastDelete bool
}

type Record struct {
	BufferList     [2][]byte
	PieceList      []Piece
	PieceIndexTree IdxTree
	History        History
	TotalChars     IDX
	Caret          Caret
	Flags          FLAGS
}

func (r *Record) NewRecord() *Record {
	record := &Record{
		BufferList: [2][]byte{make([]byte, 0), make([]byte, 0, BufferExtensionSize)},
		PieceList:  make([]Piece, 0, PieceListExtensionCount),
		History: History{
			Ops: make([]Operation, 0, HistoryExtensionCount),
		},
	}
	initNode := &IdxNode{
		Parent:   nil,
		Children: [2]*IdxNode{nil, nil},
		Value:    0,
	}
	record.PieceIndexTree = IdxTree{
		Root:      initNode,
		NodeCount: 1,
	}
	initPiece := Piece{
		Buffer:              0,
		Start:               0,
		End:                 0,
		Chars:               0,
		CharsOnLeftChildren: 0,
	}
	record.PieceList = append(record.PieceList, initPiece)
	return record
}

func (r *Record) ExtendAddBuffer(atLeast IDX) {
	extendSize := BufferExtensionSize
	if int(atLeast) > extendSize {
		extendSize = int(atLeast)
	}
	newBuf := make([]byte, len(r.BufferList[1]), len(r.BufferList[1])+BufferExtensionSize)
	copy(newBuf, r.BufferList[1])
	r.BufferList[1] = newBuf
}

func (r *Record) ConsolidateBuffers() {
	newBuf := make([]byte, len(r.BufferList[InitBuf])+len(r.BufferList[AddBuf]))
	initLen := copy(newBuf, r.BufferList[InitBuf])
	copy(newBuf[initLen:], r.BufferList[AddBuf])
	for i := range r.PieceList {
		piece := r.PieceList[i]
		if piece.Buffer == AddBuf {
			piece.Start += IDX(initLen)
			piece.End += IDX(initLen)
			piece.Buffer = InitBuf
			r.PieceList[i] = piece
		}
	}
	r.BufferList[InitBuf] = newBuf
	r.BufferList[AddBuf] = make([]byte, 0, BufferExtensionSize)
}

func (r *Record) DefragmentBuffers() {
	newBuf := make([]byte, len(r.BufferList[0])+len(r.BufferList[1]))
	idx := 0
	r.PieceIndexTree.Traverse(func(node *IdxNode, depth uint64) {
		slice := r.GetNodeSlice(node)
		n := copy(newBuf[idx:], slice)
		r.PieceList[node.Value].Start = IDX(idx)
		r.PieceList[node.Value].End = IDX(idx + n)
		r.PieceList[node.Value].Buffer = InitBuf
		idx += n
	})
	r.BufferList[InitBuf] = newBuf
	r.BufferList[AddBuf] = make([]byte, 0, BufferExtensionSize)
}

func (r *Record) EraseHistory() {
	r.DefragmentBuffers()
	r.PieceList = make([]Piece, 0, PieceListExtensionCount)
	r.History = History{
		Ops: make([]Operation, 0, HistoryExtensionCount),
	}
	newRoot := r.BlankPiece()
	r.PieceList[newRoot.Value].Buffer = InitBuf
	r.PieceList[newRoot.Value].Start = 0
	r.PieceList[newRoot.Value].End = idxLen(r.BufferList[InitBuf])
	r.PieceList[newRoot.Value].Chars = r.TotalChars
	r.PieceList[newRoot.Value].CharsOnLeftChildren = 0
	r.PieceIndexTree.Root = newRoot
	r.PieceIndexTree.NodeCount = 1
}

func (r *Record) BlankPiece() *IdxNode {
	if len(r.PieceList) == cap(r.PieceList) {
		newPieceList := make([]Piece, len(r.PieceList), cap(r.PieceList)+PieceListExtensionCount)
		copy(newPieceList, r.PieceList)
		r.PieceList = newPieceList
	}
	idx := len(r.PieceList)
	newPiece := Piece{
		Buffer:              NoBuf,
		Start:               0,
		End:                 0,
		Chars:               0,
		CharsOnLeftChildren: 0,
	}
	r.PieceList = append(r.PieceList, newPiece)
	newNode := &IdxNode{
		Parent:   nil,
		Children: [2]*IdxNode{nil, nil},
		Value:    IDX(idx),
	}
	return newNode
}

func (r *Record) ActivateBlankPiece(node *IdxNode) {
	if node == nil || r.PieceList[node.Value].Buffer != NoBuf {
		return
	}
	r.PieceList[node.Value].Buffer = AddBuf
	r.PieceList[node.Value].Start = idxLen(r.BufferList[AddBuf])
	r.PieceList[node.Value].End = idxLen(r.BufferList[AddBuf])
}

func (r *Record) TouchAndSnap(node *IdxNode, preOp *PreOpSnapshot) {
	if node == nil || preOp == nil {
		return
	}
	if _, nodeTouched := (*preOp)[node]; !nodeTouched {
		(*preOp)[node] = r.GetPieceSnap(node)
	}
}

func (r *Record) GetPieceSnap(node *IdxNode) PieceSnap {
	if node == nil {
		return PieceSnap{}
	}
	var pIdx, lIdx, rIdx IDX
	if node.Parent == nil {
		pIdx = 0
	}
	if node.Children[Left] == nil {
		lIdx = 0
	}
	if node.Children[Right] == nil {
		rIdx = 0
	}
	return PieceSnap{
		Index:       node.Value,
		Start:       r.PieceList[node.Value].Start,
		End:         r.PieceList[node.Value].End,
		Chars:       r.PieceList[node.Value].Chars,
		CharsOnLeft: r.PieceList[node.Value].CharsOnLeftChildren,
		Parent:      pIdx,
		Children:    [2]IDX{lIdx, rIdx},
	}
}

func (r *Record) AddPieceBefore(node *IdxNode, preOp *PreOpSnapshot) *IdxNode {
	return r.AddPieceOnSide(node, Left, preOp)
}
func (r *Record) AddPieceAfter(node *IdxNode, preOp *PreOpSnapshot) *IdxNode {
	return r.AddPieceOnSide(node, Right, preOp)
}

func (r *Record) AddPieceOnSide(node *IdxNode, side NodeSide, preOp *PreOpSnapshot) *IdxNode {
	if node == nil || preOp == nil {
		return nil
	}
	newNode := r.BlankPiece()
	r.TouchAndSnap(newNode, preOp)
	r.TouchAndSnap(node, preOp)
	sideNode := node.Children[side]
	if sideNode != nil {
		r.TouchAndSnap(sideNode, preOp)
		sideNode.Parent = newNode
		newNode.Children[side] = sideNode
	}
	node.Children[side] = newNode
	newNode.Parent = node
	r.PieceIndexTree.NodeCount += 1
	if side == Left {
		r.PieceList[newNode.Value].CharsOnLeftChildren = r.PieceList[newNode.Children[Left].Value].CharsPlusCharsBelowOnLeft()
	}
	return newNode
}

func (r *Record) SplitPiece(node *IdxNode, charOffset IDX, preOp *PreOpSnapshot) (newRightNode *IdxNode) {
	if node == nil || preOp == nil {
		return nil
	}
	piece := r.PieceList[node.Value]
	if charOffset < 0 {
		return r.AddPieceBefore(node, preOp)
	}
	if charOffset >= piece.Len() {
		return r.AddPieceAfter(node, preOp)
	}
	// FIND BYTE SPLIT
	sum := IDX(0)
	cutOffset := charOffset
	slice := r.BufferList[piece.Buffer][piece.Start : piece.Start+cutOffset]
	lastRune, _ := utf8.DecodeLastRune(slice)
	for lastRune == utf8.RuneError {
		cutOffset -= 1
		slice = r.BufferList[piece.Buffer][piece.Start : piece.Start+cutOffset]
		lastRune, _ = utf8.DecodeLastRune(slice)
	}
	sum += IDX(utf8.RuneCount(slice))
	for sum < charOffset && cutOffset < piece.Len() {
		_, rSize := utf8.DecodeRune(r.BufferList[piece.Buffer][piece.Start+cutOffset:])
		sum += 1
		cutOffset += uint32(rSize)
	}
	if cutOffset >= piece.Len() {
		return r.AddPieceAfter(node, preOp)
	}
	// DO CHANGES
	r.TouchAndSnap(node, preOp)
	splitChars := piece.Chars - sum
	oldEnd := piece.End
	r.PieceList[node.Value].End = cutOffset
	r.PieceList[node.Value].Chars = sum
	r.BubbleUpCharChange(node, splitChars, Delete, preOp)
	newRightNode = r.BlankPiece()
	r.TouchAndSnap(newRightNode, preOp)
	r.PieceList[newRightNode.Value].Buffer = r.PieceList[node.Value].Buffer
	r.PieceList[newRightNode.Value].Start = cutOffset
	r.PieceList[newRightNode.Value].End = oldEnd
	r.PieceList[newRightNode.Value].Chars = splitChars
	rightChild := node.Children[Right]
	if rightChild != nil {
		r.TouchAndSnap(rightChild, preOp)
		rightChild.Parent = newRightNode
		newRightNode.Children[Right] = rightChild
	}
	node.Children[Right] = newRightNode
	newRightNode.Parent = node
	r.PieceIndexTree.NodeCount += 1
	return newRightNode
}

func (r *Record) GetNodeSlice(node *IdxNode) []byte {
	if node == nil {
		return nil
	}
	piece := r.PieceList[node.Value]
	if piece.Buffer == NoBuf {
		return nil
	}
	buffer := r.BufferList[piece.Buffer]
	return buffer[piece.Start:piece.End]
}

func (r *Record) SeekCaret(charIndex IDX) {
	if r.Caret.CharIdx == charIndex {
		return
	}
	r.History.CaretMoveSinceLastDelete = true
	node, charsBefore := r.FindRelevantNode(charIndex)
	r.Caret.Node = node
	r.Caret.NodeCharStart = charsBefore
	r.Caret.NodeCharEnd = charsBefore + r.PieceList[node.Value].Chars
	r.Caret.CharIdx = charIndex
}

func (r *Record) MoveCaret(charCount IDX, direction NodeSide) {
	if charCount == 0 {
		return
	}
	r.History.CaretMoveSinceLastDelete = true
	moveRemaining := charCount
	for moveRemaining > 0 {
		max := IDX(0)
		switch direction {
		case Left:
			max = r.Caret.CharIdx - r.Caret.NodeCharStart
			if max < moveRemaining {
				moveRemaining -= max
				r.Caret.Node = r.Caret.Node.Prev()
				r.Caret.CharIdx = r.Caret.CharIdx - max
				r.Caret.NodeCharStart = r.PieceList[r.Caret.Node.Value].CharsOnLeftChildren
				r.Caret.NodeCharEnd = r.Caret.NodeCharStart + r.PieceList[r.Caret.Node.Value].Chars
			} else {
				r.Caret.CharIdx -= charCount
				return
			}
		case Right:
			max = r.Caret.NodeCharEnd - r.Caret.CharIdx
			if max < moveRemaining {
				moveRemaining -= max
				r.Caret.Node = r.Caret.Node.Next()
				r.Caret.CharIdx = r.Caret.CharIdx + max
				r.Caret.NodeCharStart = r.PieceList[r.Caret.Node.Value].CharsOnLeftChildren
				r.Caret.NodeCharEnd = r.Caret.NodeCharStart + r.PieceList[r.Caret.Node.Value].Chars
			} else {
				r.Caret.CharIdx += charCount
				return
			}
		}
	}
}

func (r *Record) FindRelevantNode(charIndex IDX) (node *IdxNode, charsBeforeNode IDX) {
	if r.Caret.Node != nil && charIndex > r.Caret.NodeCharStart && charIndex <= r.Caret.NodeCharEnd {
		return r.Caret.Node, r.Caret.NodeCharStart
	}
	if r.PieceIndexTree.Root == nil {
		return nil, 0
	}
	node = r.PieceIndexTree.Root
	leftSum := IDX(0)
	match := false
	for !match {
		if node.Children[Left] != nil && charIndex <= leftSum+r.PieceList[node.Value].CharsOnLeftChildren {
			node = node.Children[Left]
		} else if charsOnNodePlusBelowLeft := r.PieceList[node.Value].CharsPlusCharsBelowOnLeft(); node.Children[Right] != nil && charIndex > leftSum+charsOnNodePlusBelowLeft {
			node = node.Children[Right]
			leftSum += charsOnNodePlusBelowLeft
		} else {
			match = true
			leftSum += r.PieceList[node.Value].CharsOnLeftChildren
		}
	}
	return node, leftSum
}

func (r *Record) LastOpCanBeCombined(pieceIdx IDX, opType OP, charCount IDX) bool {
	if ((opType == Write && pieceIdx == r.History.LastOpPieceIdx && r.PieceList[pieceIdx].Buffer == AddBuf) ||
		(opType == Delete && pieceIdx == r.History.LastDeleteEndIdx && !r.History.CaretMoveSinceLastDelete)) &&
		opType == r.History.LastOpType &&
		r.History.LastOpChars+charCount < OpCombineMaxChars &&
		r.History.ActiveIndex == idxLen(r.History.Ops) &&
		r.History.LastOpTime.Add(OpCombineTimer).After(time.Now()) {
		return true
	}
	return false
}

func (r *Record) CombineOpWithLast(op Operation) {
	lastOp := r.History.Ops[r.History.ActiveIndex]
	for thisIdx := range op.Before {
		has := false
		lastIdx := 0
		for lastIdx = range lastOp.Before {
			if op.Before[thisIdx].Index == lastOp.Before[lastIdx].Index {
				has = true
				break
			}
		}
		if !has {
			lastOp.Before = append(lastOp.Before, op.Before[thisIdx])
		}
		lastOp.After[lastIdx] = op.After[thisIdx]
	}
	r.History.Ops[r.History.ActiveIndex] = lastOp
}

func (r *Record) WriteToAddBuffer(chars []rune) IDX {
	totalLen := 0
	for _, c := range chars {
		totalLen += utf8.RuneLen(c)
	}
	start := len(r.BufferList[AddBuf])
	end := totalLen + start
	if end > cap(r.BufferList[AddBuf]) {
		r.ExtendAddBuffer(uint32(totalLen))
	}
	r.BufferList[AddBuf] = r.BufferList[AddBuf][:end]
	for _, c := range chars {
		start += utf8.EncodeRune(r.BufferList[AddBuf][start:], c)
	}
	return IDX(end)
}

func (r *Record) WriteChars(charIndex IDX, chars []rune) {
	node, charsBeforeNode := r.FindRelevantNode(charIndex)
	offset := charIndex - charsBeforeNode
	preOp := make(PreOpSnapshot)
	piece := r.PieceList[node.Value]
	charCount := idxLen(chars)
	if offset == piece.Chars && idxLen(r.BufferList[AddBuf]) == piece.End { // Case: Extend last created piece
		r.TouchAndSnap(node, &preOp)
		newEnd := r.WriteToAddBuffer(chars)
		r.PieceList[node.Value].End = newEnd
		r.PieceList[node.Value].Chars += charCount
		r.BubbleUpCharChange(node, charCount, Write, &preOp)
		op := r.FinalizeOperation(Write, &preOp)
		if r.LastOpCanBeCombined(node.Value, Write, charCount) {
			r.CombineOpWithLast(op)
		} else {
			r.AppendOpToHistory(op)
		}
	} else { // Case: add new piece
		if offset != piece.Chars { // Case: Split original piece in two first
			r.SplitPiece(node, offset, &preOp)
		}
		newNode := r.AddPieceAfter(node, &preOp)
		// newNode already touched
		end := r.WriteToAddBuffer(chars)
		r.PieceList[newNode.Value].End = end
		r.PieceList[newNode.Value].Start = end - charCount
		r.PieceList[newNode.Value].Chars += charCount
		r.BubbleUpCharChange(newNode, charCount, Write, &preOp)
		op := r.FinalizeOperation(Write, &preOp)
		// cannot combine ops
		r.AppendOpToHistory(op)
	}
	r.Flags |= NeedsRedraw | NeedsSave
}

func (r *Record) DeleteChars(charIndex IDX, charCount IDX) {
	node, charsBeforeNode := r.FindRelevantNode(charIndex)
	startNode := node
	offset := charIndex - charsBeforeNode
	preOp := make(PreOpSnapshot)
	piece := r.PieceList[node.Value]
	deleted := IDX(0)
	for deleted < charCount {
		r.TouchAndSnap(node, &preOp)
		charsThisLoop := IDX(0)
		if offset != piece.Chars {
			byteOff := 0
			slice := r.GetNodeSlice(node)
			for charsThisLoop < offset {
				_, n := utf8.DecodeRune(slice[byteOff:])
				byteOff += n
				charsThisLoop += 1
			}
			r.PieceList[node.Value].Start += IDX(byteOff)
			r.PieceList[node.Value].Chars -= charsThisLoop
			deleted += charsThisLoop
		} else {
			charsThisLoop = piece.Chars
			rem := charCount - deleted
			if rem < charsThisLoop {
				charsThisLoop = rem
				byteEnd := piece.End - piece.Start
				slice := r.GetNodeSlice(node)
				for rem > 0 {
					_, n := utf8.DecodeLastRune(slice[:byteEnd])
					byteEnd -= IDX(n)
					rem -= 1
				}
				r.PieceList[node.Value].End = piece.Start - byteEnd
				r.PieceList[node.Value].Chars -= charsThisLoop
				deleted += charsThisLoop
			} else {
				r.PieceList[node.Value].End = piece.Start
				r.PieceList[node.Value].Chars = 0
				deleted += charsThisLoop
			}
		}
		r.BubbleUpCharChange(node, charsThisLoop, Delete, &preOp)
		if node.Prev() == nil {
			break
		}
		node = node.Prev()
		piece = r.PieceList[node.Value]
		offset = piece.Chars
	}
	op := r.FinalizeOperation(Delete, &preOp)
	if r.LastOpCanBeCombined(startNode.Value, Delete, charCount) {
		r.CombineOpWithLast(op)
	} else {
		r.AppendOpToHistory(op)
	}
	r.History.LastDeleteEndIdx = node.Value
	r.History.CaretMoveSinceLastDelete = false
	r.Flags |= NeedsSave | NeedsRedraw
}

func (r *Record) BubbleUpCharChange(node *IdxNode, charCount IDX, opType OP, preOp *PreOpSnapshot) {
	if node == nil || opType == NoOp || opType == Redaction || charCount == 0 || preOp == nil {
		return
	}
	for node.Parent != nil {
		if node.Side() == Left {
			r.TouchAndSnap(node.Parent, preOp)
			count := r.PieceList[node.Parent.Value].CharsOnLeftChildren
			if opType == Write {
				count += charCount
			} else if opType == Delete {
				count -= charCount
			}
			r.PieceList[node.Parent.Value].CharsOnLeftChildren = count
		}
		node = node.Parent
	}
}

func (r *Record) FinalizeOperation(opType OP, preOp *PreOpSnapshot) Operation {
	count := len(*preOp)
	op := Operation{
		Type:   opType,
		Before: make(OpSnap, 0, count),
		After:  make(OpSnap, 0, count),
	}
	for node, snap := range *preOp {
		op.Before = append(op.Before, snap)
		op.After = append(op.After, r.GetPieceSnap(node))
	}
	return op
}

func (r *Record) AppendOpToHistory(op Operation) {
	if op.Type == NoOp {
		return
	}
	if len(r.History.Ops)+2 > cap(r.History.Ops) {
		newOps := make([]Operation, len(r.History.Ops), len(r.History.Ops)+HistoryExtensionCount)
		copy(newOps, r.History.Ops)
		r.History.Ops = newOps
	}
	if r.History.ActiveIndex != idxLen(r.History.Ops) {
		redaction := r.RedactHistory()
		if redaction.Type != NoOp {
			r.History.Ops = append(r.History.Ops, redaction)
		}
	}
	r.History.Ops = append(r.History.Ops, op)
	r.History.ActiveIndex = idxLen(r.History.Ops)
}

func (r *Record) RedactHistory() Operation {
	if r.History.ActiveIndex == idxLen(r.History.Ops) {
		return noop
	}
	start := r.History.ActiveIndex
	beforeTouched := make(map[IDX]struct{})
	afterTouched := make(map[IDX]struct{})
	op := Operation{
		Type:   Redaction,
		Before: make(OpSnap, 0, 10),
		After:  make(OpSnap, 0, 10),
	}
	for i := start; i < idxLen(r.History.Ops); i += 1 {
		for _, snap := range r.History.Ops[i].Before {
			if _, exists := beforeTouched[snap.Index]; !exists {
				beforeTouched[snap.Index] = struct{}{}
				op.After = append(op.After, snap)
			}
		}
	}
	for i := idxLen(r.History.Ops) - 1; i >= start; i -= 1 {
		for _, snap := range r.History.Ops[i].After {
			if _, exists := afterTouched[snap.Index]; !exists {
				afterTouched[snap.Index] = struct{}{}
				op.Before = append(op.Before, snap)
			}
		}
	}
	opLen := len(op.Before)
	op.Before = op.Before[:opLen:opLen]
	op.After = op.After[:opLen:opLen]
	return op
}

//TODO: MatchNodesToSnapshot(), Undo(), Redo()

func idxLen[T any](slice []T) IDX {
	return IDX(len(slice))
}

var noop = Operation{
	Type: NoOp,
}
