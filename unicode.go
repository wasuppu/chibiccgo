package main

// Encode a given character in UTF-8.
func encodeUtf8(buf []byte, c uint32) int {
	if c <= 0x7F {
		buf[0] = byte(c)
		return 1
	}

	if c <= 0x7FF {
		buf[0] = 0b11000000 | byte(c>>6)
		buf[1] = 0b10000000 | byte(c&0b00111111)
		return 2
	}

	if c <= 0xFFFF {
		buf[0] = 0b11100000 | byte(c>>12)
		buf[1] = 0b10000000 | byte((c>>6)&0b00111111)
		buf[2] = 0b10000000 | byte(c&0b00111111)
		return 3
	}

	buf[0] = 0b11110000 | byte(c>>18)
	buf[1] = 0b10000000 | byte((c>>12)&0b00111111)
	buf[2] = 0b10000000 | byte((c>>6)&0b00111111)
	buf[3] = 0b10000000 | byte(c&0b00111111)
	return 4
}

// Read a UTF-8-encoded Unicode code point from a source file.
// We assume that source files are always in UTF-8.
//
// UTF-8 is a variable-width encoding in which one code point is
// encoded in one to four bytes. One byte UTF-8 code points are
// identical to ASCII. Non-ASCII characters are encoded using more
// than one byte.
func decodeUtf8(newPos *int, p int) uint32 {
	if source[p] < 128 {
		*newPos = p + 1
		return uint32(source[p])
	}

	start := p
	var len int
	var c uint32

	if source[p] >= 0b11110000 {
		len = 4
		c = uint32(source[p] & 0b111)
	} else if source[p] >= 0b11100000 {
		len = 3
		c = uint32(source[p] & 0b1111)
	} else if source[p] >= 0b11000000 {
		len = 2
		c = uint32(source[p] & 0b11111)
	} else {
		failAt(start, "invalid UTF-8 sequence")
	}

	for i := 1; i < len; i++ {
		if source[p+i]>>6 != 0b10 {
			failAt(start, "invalid UTF-8 sequence")
		}
		c = (c << 6) | uint32(source[p+i]&0b111111)
	}

	*newPos = p + len
	return c
}
