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
