package docrecord_test

import "testing"

func BenchmarkBufferExtension(b *testing.B) {
	a := make([]byte, 10000)
	a[0] = 1
	a[9999] = 1
	for i := 0; i < b.N; i += 1 {
		b := make([]byte, 10000, 12000)
		copy(b, a)
	}
}

func TestPieceAleration(t *testing.T) {
	type val struct {
		num int
	}
	master := struct {
		list []val
	}{
		list: make([]val, 1),
	}
	a := master.list[0].num
	master.list[0].num = 2
	b := master.list[0].num
	if a == b || b != 2 {
		t.Error("change to value did not propogate")
	}
}
