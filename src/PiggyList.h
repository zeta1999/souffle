#pragma once

#include <iostream>
#include <atomic>
#include <list>
#include "ParallelUtils.h"
//#include "spinny_lock.h"
//#include <concurrentqueue.h>

using std::size_t;
namespace souffle {


template <class T>
class PiggyList {
    const size_t BLOCKBITS = 16ul;
    const size_t BLOCKSIZE = (1ul << BLOCKBITS);

    std::atomic<size_t> m_size;
    // how large each new allocation will be 
    size_t allocsize = BLOCKSIZE;
    std::atomic<size_t> container_size;
    std::atomic<size_t> num_containers;

    // 2^64 elements can be stored (default initialise to nullptrs)
    static constexpr size_t max_conts = 64;
    T* blockLookupTable[max_conts]{};

    // for parallel node insertions
    mutable SpinLock sl;

    /**
     * Free the arrays allocated within the linked list nodes
     */
    void freeList() {
        for (size_t i = 0; i < num_containers.load(); ++i) delete[] blockLookupTable[i];
    }

public:
    PiggyList() {
        for (size_t i = 0; i < max_conts; ++i) blockLookupTable[i] = nullptr;
        m_size.store(0);
        num_containers.store(0);
        container_size.store(0);
    }

    PiggyList(size_t initialbitsize) : BLOCKBITS(initialbitsize) {
        for (size_t i = 0; i < max_conts; ++i) blockLookupTable[i] = nullptr;
        m_size.store(0);
        container_size.store(0);
        num_containers.store(0);
    }

    /** copy constructor */
    PiggyList(const PiggyList& other) = delete;

    /** move constructor */
    PiggyList(PiggyList&& other) = delete;

    PiggyList& operator=(PiggyList other) = delete;

    ~PiggyList() {
        freeList();
    }

    /**
     * Well, returns the number of nodes exist within the list + number of nodes queued to be inserted
     *  The reason for this, is that there may be many nodes queued up
     *  that haven't had time to had containers created and updated
     * @return the number of nodes exist within the list + number of nodes queued to be inserted
     */
    inline size_t size() const {
        return m_size.load();
    };
    
    inline size_t containerSize() const {
        return container_size.load();
    }


    inline T* getBlock(size_t blocknum) const {
        return this->blockLookupTable[blocknum];
    }

    size_t append(T element) {
        size_t new_index = m_size.fetch_add(1, std::memory_order_acquire);

        // will this not fit?
        if (container_size < new_index + 1) {
            sl.lock();
            // check and add as many containers as required
            while (container_size < new_index + 1) {
                blockLookupTable[num_containers] = new T[allocsize];
                num_containers += 1;
                container_size += allocsize;
                // double the number elements that will be allocated next time
                allocsize <<= 1;
            }
            sl.unlock();
        }
        
        this->get(new_index) = element;
        return new_index;
    }

    size_t createNode() {
        size_t new_index = m_size.fetch_add(1, std::memory_order_acquire);

        // will this not fit?
        if (container_size < new_index + 1) {
            sl.lock();
            // check and add as many containers as required
            while (container_size < new_index + 1) {
                blockLookupTable[num_containers] = new T[allocsize];
                num_containers += 1;
                container_size += allocsize;
                // double the number elements that will be allocated next time
                allocsize <<= 1;
            }
            sl.unlock();
        }
        
        return new_index;
    }

//    /**
//     * Create/reserve room for an element, and return its index so it can be written to
//     */
//    size_t createNode() {
//
//        size_t new_index = m_size.fetch_add(1, std::memory_order_relaxed);
//
//        // spin and try and insert the node at the correct index (we may need to construct a new block)
//
//        // if we don't have a valid index to store this element
//        if (container_size.load() < new_index + 1){
//
//            sl.lock();
//
//            // double check & add as many blocks as necessary 
//            // (although, I do hope this never loops multiple times, as it means there's at least 
//            //      BLOCKSIZE threads concurrently writing...)
//            while (container_size.load() < new_index + 1) {
//                listData.push_back(new T[allocsize]);
//                //update lookup table
//                this->blockLookupTable[listData.size() - 1] = listData.back();
//                container_size += allocsize;
//                // next time our linked list node will have twice the capacity
//                allocsize <<= 1;
//            }
//            
//            sl.unlock(); 
//        }
//        
//        return new_index;
//    }
//
    ///**
    // * Insert a value at a specific index, expanding the data structure if necessary.
    // *  Warning: this will "waste" inbetween elements, if you do not know their index,
    // *      and do not set them with insertAt
    // * @param index  the index inside the blocklist
    // * @param value  the value to set inside that index
    // */
    //void insertAt(size_t index, T value) {
    //    //size_t new_index = m_size.fetch_add(1, std::memory_order_acquire);

    //    //// will this not fit?
    //    //if (container_size < new_index + 1) {
    //    //    sl.lock();
    //    //    // check and add as many containers as required
    //    //    while (container_size < new_index + 1) {
    //    //        blockLookupTable[num_containers] = new T[allocsize];
    //    //        num_containers += 1;
    //    //        container_size += allocsize;
    //    //        // double the number elements that will be allocated next time
    //    //        allocsize <<= 1;
    //    //    }
    //    //    sl.unlock();
    //    //}
    //    //
    //    //return new_index;
    //    
    //    size_t nindex = index + BLOCKSIZE;
    //    size_t blockNum = (63 - __builtin_clzll(nindex)) - BLOCKBITS;

    //    if (blockLookupTable[blockNum] == nullptr) {

    //    }

    //    // exact same logic as createNode, but instead just cares about expanding til our index fits
    //    // not if a newly added node will fit.
    //    while (container_size.load() < index + 1) {
    //        sl.lock();
    //        // just in case
    //        while (container_size.load() < index + 1) {
    //            blockLookupTable[num_containers] = new T[allocsize];
    //            num_containers += 1;
    //            container_size += allocsize;
    //            // double the number elements that will be allocated next time
    //            allocsize <<= 1;
    //        }

    //        sl.unlock();
    //    }

    //    this->get(index) = value;
    //}

//    /** 
//     * A function that you probably shouldn't be calling. (Used by the hashmap)
//     * Adds another block to our underlying container
//     * @return the size of the container that it at least is
//     */
//    size_t addBlock() {
//        sl.lock();
//
//        listData.push_back(new T[allocsize]());
//        this->blockLookupTable[listData.size() - 1] = listData.back();
//        container_size += allocsize;
//        // double the size of the next allocated block
//        allocsize <<= 1;
//
//        sl.unlock();
//        
//        return container_size.load();
//    }

    /**
     * Retrieve a reference to the stored value at index
     * @param index position to search
     * @return the value at index
     */
    // XXX: in another commit i made this const size_t&  - is this really that beneficial?
    inline T& get(size_t index) const {
        // supa fast 2^16 size first block
        size_t nindex = index + BLOCKSIZE;
        size_t blockNum = (63 - __builtin_clzll(nindex));
        size_t blockInd = (nindex) & ((1 << blockNum) - 1);
        return this->getBlock(blockNum-BLOCKBITS)[blockInd];
    }

    /**
     * Clear all elements from the PiggyList
     */
    void clear() {
        freeList();
        m_size = 0;
        container_size = 0;
    }

    class iterator : std::iterator<std::forward_iterator_tag, T> {
        size_t cIndex = 0;
        PiggyList* bl;

    public:
        // default ctor, to silence
        iterator(){};

        /* begin iterator for iterating over all elements */
        iterator(PiggyList* bl) : bl(bl){};
        /* ender iterator for marking the end of the iteration */
        iterator(PiggyList* bl, size_t beginInd) : cIndex(beginInd), bl(bl){};

        T operator*() {
            return bl->get(cIndex);
        };
        const T operator*() const {
            return bl->get(cIndex);
        };

        iterator& operator++(int) {
            ++cIndex;
            return *this;
        };

        iterator operator++() {
            iterator ret(*this);
            ++cIndex;
            return ret;
        };

        friend bool operator==(const iterator& x, const iterator& y) {
            return x.cIndex == y.cIndex && x.bl == y.bl;
        };

        friend bool operator!=(const iterator& x, const iterator& y) {
            return !(x == y);
        };
    };

    iterator begin() {
        return iterator(this);
    };
    iterator end() {
        return iterator(this, size());
    };
};

template <class T>
class ST_PiggyList {
    const size_t BLOCKBITS = 16ul;
    const size_t BLOCKSIZE = (1ul << BLOCKBITS);

    size_t m_size;
    // how large each new allocation will be 
    size_t allocsize = BLOCKSIZE;
    size_t container_size;
    size_t num_containers;

    // 2^64 elements can be stored (default initialise to nullptrs)
    static constexpr size_t max_conts = 64;
    T* blockLookupTable[max_conts]{};

    /**
     * Free the arrays allocated within the linked list nodes
     */
    void freeList() {
        for (size_t i = 0; i < num_containers; ++i) delete[] blockLookupTable[i];
    }

public:
    ST_PiggyList() {
        for (int i = 0; i < max_conts; ++i) blockLookupTable[i] = nullptr;
        m_size = 0;
        num_containers = 0;
        container_size = 0;
    }

    ST_PiggyList(size_t initialbitsize) : BLOCKBITS(initialbitsize) {
        for (size_t i = 0; i < max_conts; ++i) blockLookupTable[i] = nullptr;
        m_size = 0;
        num_containers = 0;
        container_size = 0;
    }

    /** copy constructor */
    ST_PiggyList(const ST_PiggyList& other) = delete;

    /** move constructor */
    ST_PiggyList(ST_PiggyList&& other) = delete;

    ST_PiggyList& operator=(ST_PiggyList other) = delete;

    ~ST_PiggyList() {
        freeList();
    }

    /**
     * Well, returns the number of nodes exist within the list + number of nodes queued to be inserted
     *  The reason for this, is that there may be many nodes queued up
     *  that haven't had time to had containers created and updated
     * @return the number of nodes exist within the list + number of nodes queued to be inserted
     */
    inline size_t size() const {
        return m_size;
    };
    
    inline size_t containerSize() const {
        return container_size;
    }


    inline T* getBlock(size_t blocknum) const {
        return this->blockLookupTable[blocknum];
    }

    size_t append(T element) {
        size_t new_index = m_size++;

        // will this not fit?
        if (__builtin_expect((container_size < new_index + 1), 0)) {
            blockLookupTable[num_containers] = new T[allocsize];
            num_containers += 1;
            container_size += allocsize;
            // double the number elements that will be allocated next time
            allocsize <<= 1;
        }
        
        this->get(new_index) = element;
        return new_index;
    }


    /**
     * Retrieve a reference to the stored value at index
     * @param index position to search
     * @return the value at index
     */
    inline T& get(size_t index) const {
        // supa fast 2^16 size first block
        size_t nindex = index + BLOCKSIZE;
        size_t blockNum = (63 - __builtin_clzll(nindex));
        size_t blockInd = (nindex) & ((1 << blockNum) - 1);
        return this->getBlock(blockNum-BLOCKBITS)[blockInd];
    }

    /**
     * Clear all elements from the ST_PiggyList
     */
    void clear() {
        freeList();
        m_size = 0;
        container_size = 0;
    }

    class iterator : std::iterator<std::forward_iterator_tag, T> {
        size_t cIndex = 0;
        ST_PiggyList* bl;

    public:
        // default ctor, to silence
        iterator(){};

        /* begin iterator for iterating over all elements */
        iterator(ST_PiggyList* bl) : bl(bl){};
        /* ender iterator for marking the end of the iteration */
        iterator(ST_PiggyList* bl, size_t beginInd) : cIndex(beginInd), bl(bl){};

        T operator*() {
            return bl->get(cIndex);
        };
        const T operator*() const {
            return bl->get(cIndex);
        };

        iterator& operator++(int) {
            ++cIndex;
            return *this;
        };

        iterator operator++() {
            iterator ret(*this);
            ++cIndex;
            return ret;
        };

        friend bool operator==(const iterator& x, const iterator& y) {
            return x.cIndex == y.cIndex && x.bl == y.bl;
        };

        friend bool operator!=(const iterator& x, const iterator& y) {
            return !(x == y);
        };
    };

    iterator begin() {
        return iterator(this);
    };
    iterator end() {
        return iterator(this, size());
    };
};


/* 
 * Slightly more optimised single threaded piggylist.
 * Always starts at size 1.
 */
template <class T>
class SingleThreadPiggyList {
    size_t m_size;
    // how large each new allocation will be 
    size_t container_size;
    size_t num_containers;

    // 2^64 elements can be stored (default initialise to nullptrs)
    static constexpr size_t max_conts = 64;
    T* blockLookupTable[max_conts]{};

    /**
     * Free the arrays allocated within the linked list nodes
     */
    void freeList() {
        for (size_t i = 0; i < num_containers; ++i) delete[] blockLookupTable[i];
    }

public:
    SingleThreadPiggyList() {
        for (int i = 0; i < max_conts; ++i) blockLookupTable[i] = nullptr;
        m_size = 0;
        num_containers = 0;
        container_size = 0;
    }

    /** copy constructor */
    SingleThreadPiggyList(const SingleThreadPiggyList& other) = delete;

    /** move constructor */
    SingleThreadPiggyList(SingleThreadPiggyList&& other) = delete;

    SingleThreadPiggyList& operator=(SingleThreadPiggyList other) = delete;

    ~SingleThreadPiggyList() {
        freeList();
    }

    /**
     * Well, returns the number of nodes exist within the list + number of nodes queued to be inserted
     *  The reason for this, is that there may be many nodes queued up
     *  that haven't had time to had containers created and updated
     * @return the number of nodes exist within the list + number of nodes queued to be inserted
     */
    inline size_t size() const {
        return m_size;
    };
    
    inline size_t containerSize() const {
        return container_size;
    }


    inline T* getBlock(size_t blocknum) const {
        return this->blockLookupTable[blocknum];
    }

    inline void append(const T element) {
        size_t new_index = m_size++;
        // will this not fit? (unlikely to be true)
        if (__builtin_expect((container_size < new_index + 1), 0)) {
            blockLookupTable[num_containers] = new T[1 << num_containers];
            container_size += 1 << num_containers;
            num_containers += 1;
        }
        
        this->get(new_index) = element;
    }


    /**
     * Retrieve a reference to the stored value at index
     * @param index position to search
     * @return the value at index
     */
    inline T& get(size_t index) const {
        const size_t new_index = index+1;
        const size_t blockNum = (63 - __builtin_clzll(new_index));
        const size_t blockInd = (new_index) ^ (1 << blockNum);
        return this->getBlock(blockNum)[blockInd];
    }

    /**
     * Clear all elements from the SingleThreadPiggyList
     */
    void clear() {
        freeList();
        m_size = 0;
        container_size = 0;
    }

    class iterator : std::iterator<std::forward_iterator_tag, T> {
        size_t cIndex = 0;
        SingleThreadPiggyList* bl;

    public:
        /* begin iterator for iterating over all elements */
        iterator(SingleThreadPiggyList* bl) : bl(bl){};
        /* ender iterator for marking the end of the iteration */
        iterator(SingleThreadPiggyList* bl, size_t beginInd) : cIndex(beginInd), bl(bl){};

        T operator*() {
            return bl->get(cIndex);
        };
        const T operator*() const {
            return bl->get(cIndex);
        };

        iterator& operator++(int) {
            ++cIndex;
            return *this;
        };

        iterator operator++() {
            iterator ret(*this);
            ++cIndex;
            return ret;
        };

        friend bool operator==(const iterator& x, const iterator& y) {
            return x.cIndex == y.cIndex && x.bl == y.bl;
        };

        friend bool operator!=(const iterator& x, const iterator& y) {
            return !(x == y);
        };
    };

    iterator begin() {
        return iterator(this);
    };
    iterator end() {
        return iterator(this, size());
    };
};

///* a blazin' fast concurrent vector - with deletion! */
//template <class T>
//class BlockListRemovable {
//    const size_t BLOCKBITS = size_t(16);
//    const size_t BLOCKSIZE = size_t(1) << BLOCKBITS;
//
//    std::atomic<size_t> m_size;
//    // how large each new allocation will be
//    std::atomic<size_t> container_size;
//    size_t num_containers = 0;
//    // how large each new allocation will be 
//    size_t allocsize = BLOCKSIZE;
//
//    // when we delete, we store 'unused' indexes here
//    moodycamel::ConcurrentQueue<size_t> removedIndexes;
//
//    static constexpr size_t MAXINDEXPOWER = 64;
//    // supports 64 node long linked list & with doubling
//    // each index points to the respective indexed linked list node
//    // a length of 64 means this data structure can store >2^64 values.
//    // depending on the starting block size (default 2^16)
//    T* blockLookupTable[MAXINDEXPOWER];
//
//    // for parallel node insertions
//    mutable SpinLock sl;
//
//    /**
//     * Free the arrays allocated within the linked list nodes
//     */
//    void freeList() {
//        for (size_t i = 0; i < num_containers; ++i) delete[] blockLookupTable[i];
//    }
//
//public:
//    BlockListRemovable() {
//        for (size_t i = 0; i < MAXINDEXPOWER; ++i) blockLookupTable[i] = nullptr;
//
//        m_size.store(0);
//        container_size.store(0);
//    }
//
//    BlockListRemovable(size_t initialbitsize) : BLOCKBITS(initialbitsize) {
//        for (size_t i = 0; i < MAXINDEXPOWER; ++i) blockLookupTable[i] = nullptr;
//
//        m_size.store(0);
//        container_size.store(0);
//    }
//
//    ~BlockListRemovable() {
//        freeList();
//    }
//
//    /**
//     * Well, returns the number of nodes exist within the list + number of nodes queued to be inserted
//     *  The reason for this, is that there may be many nodes queued up
//     *  that haven't had time to had containers created and updated
//     * @return the number of nodes exist within the list + number of nodes queued to be inserted
//     */
//    inline size_t size() const {
//        return m_size.load();
//    };
//
//    inline size_t containerSize() const {
//        return container_size.load();
//    }
//
//    inline T* getBlock(size_t blocknum) const {
//        return this->blockLookupTable[blocknum];
//    }
//
//    /**
//     * Create a value in the blocklist & return the new Node & its position.
//     * @return std::pair<New Node, Index of New Node>
//     */
//    size_t createNode() {
//        // use if we can
//        size_t new_index;
//        if (removedIndexes.try_dequeue(new_index)) {
//            return new_index;
//        }
//
//        new_index = m_size.fetch_add(1, std::memory_order_relaxed);
//
//        // spin and try and insert the node at the correct index (we may need to construct a new block)
//
//        // if we don't have a valid index to store this element
//        if (container_size.load() < new_index + 1) {
//            sl.lock();
//            // check and add as many containers as required
//            while (container_size < new_index + 1) {
//                blockLookupTable[num_containers] = new T[allocsize];
//                num_containers += 1;
//                container_size += allocsize;
//                // double the number elements that will be allocated next time
//                allocsize <<= 1;
//            }
//
//            //// double check & add as many blocks as necessary
//            //// (although, I do hope this never loops multiple times, as it means there's at least
//            ////      BLOCKSIZE threads concurrently writing...)
//            //while (container_size.load() < new_index + 1) {
//            //    blockLookupTable[num_containers] = new T[1 << num_containers];
//            //    container_size += 1 << num_containers;
//            //    ++num_containers;
//            //}
//
//            sl.unlock();
//        }
//
//        return new_index;
//    }
//
//    size_t append(T element) {
//        // use a deleted index if it exists
//        size_t new_index;
//        if (removedIndexes.try_dequeue(new_index)) {
//            this->get(new_index) = element;
//            return new_index;
//        }
//
//        new_index = m_size.fetch_add(1, std::memory_order_relaxed);
//        // will this not fit?
//        if (container_size < new_index + 1) {
//            sl.lock();
//            // check and add as many containers as required
//            while (container_size < new_index + 1) {
//                blockLookupTable[num_containers] = new T[allocsize];
//                num_containers += 1;
//                container_size += allocsize;
//                // double the number elements that will be allocated next time
//                allocsize <<= 1;
//            }
//            sl.unlock();
//        }
//
//        //// spin and try and insert the node at the correct index (we may need to construct a new block)
//
//        //// if we don't have a valid index to store this element
//        //if (container_size.load() < new_index + 1) {
//        //    sl.lock();
//
//        //    // double check & add as many blocks as necessary
//        //    // (although, I do hope this never loops multiple times, as it means there's at least
//        //    //      BLOCKSIZE threads concurrently writing...)
//        //    while (container_size.load() < new_index + 1) {
//        //        blockLookupTable[num_containers] = new T[1 << num_containers];
//        //        container_size += 1 << num_containers;
//        //        ++num_containers;
//        //    }
//
//        //    sl.unlock();
//        //}
//
//        this->get(new_index) = element;
//        return new_index;
//    }
//
//    /* blindly delete the element - if you delete an element twice, you're up shit creek */
//    void remove(size_t index) {
//        removedIndexes.enqueue(index);
//    }
//
//    /**
//     * Retrieve a reference to the stored value at index
//     * @param index position to search
//     * @return the value at index
//     */
//    T& get(size_t index) const {
//        // supa fast 2^16 size first block
//        size_t nindex = index + BLOCKSIZE;
//        size_t blockNum = (63 - __builtin_clzll(nindex));
//        size_t blockInd = (nindex) & ((1 << blockNum) - 1);
//        return this->getBlock(blockNum - BLOCKBITS)[blockInd];
//    }
//
//    /**
//     * Clear all elements from the BlockList
//     */
//    void clear() {
//        freeList();
//        m_size = 0;
//        container_size = 0;
//    }
//
//    class iterator : std::iterator<std::forward_iterator_tag, T> {
//        size_t cIndex = 0;
//        BlockListRemovable* bl;
//
//    public:
//        // default ctor, to silence
//        iterator(){};
//
//        /* begin iterator for iterating over all elements */
//        iterator(BlockListRemovable* bl) : bl(bl){};
//        /* ender iterator for marking the end of the iteration */
//        iterator(BlockListRemovable* bl, size_t beginInd) : cIndex(beginInd), bl(bl){};
//
//        T operator*() {
//            return bl->get(cIndex);
//        };
//        const T operator*() const {
//            return bl->get(cIndex);
//        };
//
//        iterator& operator++(int) {
//            ++cIndex;
//            return *this;
//        };
//
//        iterator operator++() {
//            iterator ret(*this);
//            ++cIndex;
//            return ret;
//        };
//
//        friend bool operator==(const iterator& x, const iterator& y) {
//            return x.cIndex == y.cIndex && x.bl == y.bl;
//        };
//
//        friend bool operator!=(const iterator& x, const iterator& y) {
//            return !(x == y);
//        };
//    };
//
//    iterator begin() {
//        return iterator(this);
//    };
//    iterator end() {
//        return iterator(this, size());
//    };
//};
}
