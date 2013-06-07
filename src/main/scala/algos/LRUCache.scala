package algos

import scala.collection.mutable.{LinkedList, HashMap}

/**
 * The Cache trait.
 * @param K   The type of all keys in the cache
 * @param V   The type of all values in the cache
 */
trait Cache[K, V] {
  /**
   * @param key   Input key
   * @return      Retrieve value for key if the key exists
   *              Also, readjust cache if required
   */
  def get(key: K): Option[V]
  
  /**
   * @param key    Input key
   * @param value  Value for key
   * @return       If key is not already in cache, insert the <key, value> pair
   *               If key is already in cache, some internal readjustments may be done.
   */
  def insert(key: K, value: V): Unit
}

/**
 * Simulation of an LRU cache
 * - Uses a singly linked list, and a hash map.
 * - The tail of the list is the most recently accessed key.
 * - The head of the list is the least recently used key.
 * - Each key in the hash map contains a reference to the predecessor node
 *   in the linked list
 */
case class LRUCache[K, V](val cache_capacity: Int) extends Cache[K, V] {
  
  private val hashMap = HashMap[K, Pair[V, LinkedList[K]]]()  // The hash map
  private var head_pred = LinkedList[K]() // Ref to head's predecessor node
  private var tail_pred = head_pred       // Ref to tail's predecessor node
  
  private var cache_used: Int = 0  // Number of items in cache
  
  /** 
   * Naive get implementation. Return value if its present
   * Not manipulating cache here. Leaving it for the API user to insert
   */
  override def get(key: K): Option[V] = hashMap.get(key) match {
    case Some(Pair(value, _)) => Some(value)
    case None => None
  }

  /**
   * The core of the LRU algorithm.  Insert, update happens in O(1)
   */
  override def insert(key: K, value: V): Unit = hashMap.get(key) match {
    
    // Key already exists
    case Some(Pair(_, key_pred)) =>
      move_key_to_end(key)   // Move this key to the end of the list
      hashMap.update(key, Pair(value, tail_pred))  // Update new value
    
    // Key does not exist
    case None => cache_used match {
      case x if (x == 0)             => insert_new_key_cache_empty(key, value)
      case x if (x < cache_capacity) => insert_new_key_cache_not_yet_full(key, value)
      case _                         => insert_new_key_cache_full(key, value)
    }
    
  }
  
  /**
   * move_key_to_end
   * @param  key    The key
   * @return        The node with the key is moved to the end of the linked list
   */
  private def move_key_to_end(key: K): Unit = hashMap.get(key) match {
    case Some(Pair(_, key_pred)) => { // Key already exists
      if (tail_pred != key_pred) {

        val key_ref = key_pred.next
        key_pred.next = key_pred.next.next
        
        hashMap.get(key_pred.next.head) match {
          case Some(Pair(v, _)) => hashMap.update(key_pred.next.head, Pair(v, key_pred))
          case None => /* Do nothing */
        }
        
        tail_pred = tail_pred.next
        tail_pred.next = key_ref
        tail_pred.next.next = LinkedList[K]()
      }
    }
    case None => throw new Exception("key doesn't exist") /* Key doesn't exist. Do nothing. */
  }
  
  /**
   * first_ever_insert
   * @param key    The key to be inserted in the cache
   * @param value  The value corresponding to the key
   * @return       Updates the linked list and hash table
   */
  private def insert_new_key_cache_empty(key: K, value: V): Unit = {
    tail_pred.next = LinkedList[K](key)          // Update linked list
    hashMap.put(key, Pair(value, tail_pred))     // Update hash map
    cache_used += 1                              // Increment cache_used
  }
  
  /**
   * insert_new_key_cache_not_yet_full
   * @param key   The key to be inserted in the cache
   * @param value The value corresponding to the key
   * @return      Update the linked list and hash table
   */
  private def insert_new_key_cache_not_yet_full(key: K, value: V): Unit = {
    // Linked list update
    tail_pred.next.next = LinkedList[K](key)
    tail_pred = tail_pred.next
        
    // hash update
    hashMap.put(key, Pair(value, tail_pred))
        
    cache_used += 1
  }
  
  /**
   * insert_new_key_cache_full
   * @param key   The key to be inserted in the cache
   * @param value The value corresponding to the key
   * @return      Update the linked list and hash table
   *              Since we already reached 
   */
  private def insert_new_key_cache_full(key: K, value: V): Unit = {
    // Update linked list
    val remove_key = head_pred.next.head
    
    val new_next = head_pred.next.next
    head_pred.next = new_next
        
    tail_pred.next.next = LinkedList[K](key)
    tail_pred = tail_pred.next
        
    // Update hash
    hashMap.remove(remove_key)
    hashMap.put(key, Pair(value, tail_pred))
  }

  override def toString(): String = head_pred.next.toString
}


object LRUCacheTester {
  
  def main(args: Array[String]): Unit = {
    
    val lru_cache = LRUCache[Int, String](5)
    
    println(lru_cache)
    
    lru_cache.insert(4, "four")
    println(lru_cache)
    
    lru_cache.insert(4, "four")
    println(lru_cache)
    
    lru_cache.insert(5, "five")
    println(lru_cache)
    
    lru_cache.insert(5, "five")
    println(lru_cache)
    
    lru_cache.insert(4, "four")
    println(lru_cache)
    
    lru_cache.insert(6, "six")
    println(lru_cache)
    
    lru_cache.insert(4, "four")
    println(lru_cache)
    
    lru_cache.insert(5, "five")
    println(lru_cache)
    
    lru_cache.insert(7, "seven")
    lru_cache.insert(8, "eight")
    println(lru_cache)
    
    lru_cache.insert(9, "nine")
    println(lru_cache)
    
    lru_cache.insert(9, "Nine")
    println(lru_cache.get(9))
    
    lru_cache.insert(7, "Seven")
    println(lru_cache.get(7))
  }
  
}
