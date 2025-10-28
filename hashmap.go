package main

import (
	"bytes"
	"fmt"
	"hash/fnv"
)

// Initial hash bucket size
const INIT_SIZE = 16

// Rehash if the usage exceeds 70%.
const HIGH_WATERMARK = 70

// We'll keep the usage below 50% after rehashing.
const LOW_WATERMARK = 50

// Represents a deleted hash entry
var TOMBSTONE = []byte{0xFF, 0xFF, 0xFF, 0xFF}

type HashEntry struct {
	key    []byte
	keylen int
	val    any
}

type HashMap struct {
	buckets  []HashEntry
	capacity int
	used     int
}

func fnvHash(key []byte) uint64 {
	h := fnv.New64a()
	h.Write(key)
	return h.Sum64()
}

// Make room for new entires in a given hashmap by removing
// tombstones and possibly extending the bucket size.
func rehash(m *HashMap) {
	// Compute the size of the new hashmap.
	nkeys := 0
	for i := 0; i < m.capacity; i++ {
		if m.buckets[i].key != nil && !bytes.Equal(m.buckets[i].key, TOMBSTONE) {
			nkeys++
		}
	}

	cap := m.capacity
	for (nkeys*100)/cap >= LOW_WATERMARK {
		cap *= 2
	}
	assert(cap > 0)

	// Create a new hashmap and copy all key-values.
	m2 := HashMap{
		buckets:  make([]HashEntry, cap),
		capacity: cap,
	}

	for i := 0; i < m.capacity; i++ {
		ent := m.buckets[i]
		if ent.key != nil && !bytes.Equal(ent.key, TOMBSTONE) {
			hashmapPut2(&m2, ent.key, ent.keylen, ent.val)
		}
	}

	assert(m2.used == nkeys)
	*m = m2
}

func match(ent *HashEntry, key []byte, keylen int) bool {
	return ent.key != nil && !bytes.Equal(ent.key, TOMBSTONE) &&
		ent.keylen == keylen && bytes.Equal(ent.key, key)
}

func getEntry(m *HashMap, key []byte, keylen int) *HashEntry {
	if m.buckets == nil {
		return nil
	}

	hash := fnvHash(key)

	for i := 0; i < m.capacity; i++ {
		ent := &m.buckets[(abs(int(hash))+i)%m.capacity]
		if match(ent, key, keylen) {
			return ent
		}
		if ent.key == nil {
			return nil
		}
	}
	unreachable()
	return nil
}

func getOrInsertEntry(m *HashMap, key []byte, keylen int) *HashEntry {
	if m.buckets == nil {
		m.buckets = make([]HashEntry, INIT_SIZE)
		m.capacity = INIT_SIZE
	} else if (m.used*100)/m.capacity >= HIGH_WATERMARK {
		rehash(m)
	}

	hash := fnvHash(key)

	for i := 0; i < m.capacity; i++ {
		ent := &m.buckets[(abs(int(hash))+i)%m.capacity]

		if match(ent, key, keylen) {
			return ent
		}

		if bytes.Equal(ent.key, TOMBSTONE) {
			ent.key = key
			ent.keylen = keylen
			return ent
		}

		if ent.key == nil {
			ent.key = key
			ent.keylen = keylen
			m.used++
			return ent
		}
	}
	unreachable()
	return nil
}

func hashmapGet(m *HashMap, key string) any {
	return hashmapGet2(m, []byte(key), len(key))
}

func hashmapGet2(m *HashMap, key []byte, keylen int) any {
	ent := getEntry(m, key, keylen)
	if ent != nil {
		return ent.val
	}
	return nil
}

func hashmapPut(m *HashMap, key string, val any) {
	hashmapPut2(m, []byte(key), len(key), val)
}

func hashmapPut2(m *HashMap, key []byte, keylen int, val any) {
	ent := getOrInsertEntry(m, key, keylen)
	ent.val = val
}

func hashmapDelete(m *HashMap, key string) {
	hashmapDelete2(m, []byte(key), len(key))
}

func hashmapDelete2(m *HashMap, key []byte, keylen int) {
	ent := getEntry(m, key, keylen)
	if ent != nil {
		ent.key = TOMBSTONE
	}
}

func hashmapTest() {
	m := &HashMap{}

	for i := range 5000 {
		key := fmt.Sprintf("key %d", i)
		hashmapPut(m, key, i)
	}
	for i := 1000; i < 2000; i++ {
		key := fmt.Sprintf("key %d", i)
		hashmapDelete(m, key)
	}
	for i := 1500; i < 1600; i++ {
		key := fmt.Sprintf("key %d", i)
		hashmapPut(m, key, i)
	}
	for i := 6000; i < 7000; i++ {
		key := fmt.Sprintf("key %d", i)
		hashmapPut(m, key, i)
	}

	for i := range 1000 {
		key := fmt.Sprintf("key %d", i)
		assert(hashmapGet(m, key) == i)
	}
	for i := 1000; i < 1500; i++ {
		key := "no such key"
		assert(hashmapGet(m, key) == nil)
	}
	for i := 1500; i < 1600; i++ {
		key := fmt.Sprintf("key %d", i)
		assert(hashmapGet(m, key) == i)
	}
	for i := 1600; i < 2000; i++ {
		key := "no such key"
		assert(hashmapGet(m, key) == nil)
	}
	for i := 2000; i < 5000; i++ {
		key := fmt.Sprintf("key %d", i)
		assert(hashmapGet(m, key) == i)
	}
	for i := 5000; i < 6000; i++ {
		key := "no such key"
		assert(hashmapGet(m, key) == nil)
	}
	for i := 6000; i < 7000; i++ {
		key := fmt.Sprintf("key %d", i)
		hashmapPut(m, key, i)
	}

	key := "no such key"
	assert(hashmapGet(m, key) == nil)
	fmt.Println("OK")
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
