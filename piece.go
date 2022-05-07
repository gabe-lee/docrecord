package docrecord

// func (p *Pieces) MakeMatcher() PMatch {
// 	return func(value, existing uint32) (match bool, sideNoMatch structs.NodeSide) {
// 		if p.List[value].CharsOnLeft <= p.List[existing].CharsOnLeft {
// 			return false, structs.Left
// 		}
// 		if p.List[value].CharsOnLeft > p.List[existing].CharsOnLeft {
// 			return false, structs.Right
// 		}
// 		return true, structs.Left
// 	}
// }

// func (p *Pieces) MakeSplitter() PSplit {
// 	return func(value uint32) (left uint32, right uint32) {

// 	}
// }

// func (p *Pieces) FindPiece(charIndex uint32) Piece {
// 	node := t.Root
// 	for charIndex <= node.Value.CharsOnLeft || charIndex > node.Value.CharsOnLeft+uint32(node.Value.Chars) {
// 		if charIndex <= node.Value.CharsOnLeft {
// 			node = node.Children[rb.Left]
// 		} else if charIndex > node.Value.CharsOnLeft+uint32(node.Value.Chars) {
// 			node = node.Children[rb.Right]
// 		}
// 	}
// 	return node
// }

// func (p *Piece) AddChars(numChars uint16) {
// 	p.Chars += numChars
// 	if p.family[Parent] != nil && p.OnLeft() {
// 		p.family[Parent].AddCharsOnLeft(numChars)
// 	}
// }

// func (p *rbNode) AddCharsOnLeft(numChars uint16) {
// 	p.CharsOnLeft += uint32(numChars)
// 	if p.family[Parent] != nil && p.OnLeft() {
// 		p.family[Parent].AddCharsOnLeft(numChars)
// 	}
// }

// func (p *rbNode) SubChars(numChars uint16) {
// 	p.Chars -= numChars
// 	if p.family[Parent] != nil && p.OnLeft() {
// 		p.family[Parent].SubCharsOnLeft(numChars)
// 	}
// }

// func (p *rbNode) SubCharsOnLeft(numChars uint16) {
// 	p.CharsOnLeft -= uint32(numChars)
// 	if p.family[Parent] != nil && p.OnLeft() {
// 		p.family[Parent].SubCharsOnLeft(numChars)
// 	}
// }
