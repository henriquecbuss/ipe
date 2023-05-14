/**
 * @template Key
 * @template Value
 * @returns {Map<Key, Value>}
 */
const empty = () => {
  return new Map();
};

/**
 * @template Key
 * @template Value
 * @param {Key} key
 * @returns {(value: Value) => (dict: Map<Key, Value>) => Map<Key, Value>}
 */
const insert = (key) => (value) => (dict) => dict.set(key, value);

/**
 * @template Key
 * @template Value
 * @param {Key} key
 * @returns {(dict: Map<Key, Value>) => Map<Key, Value>}
 */
const remove = (key) => (dict) => {
  dict.delete(key);
  return dict;
};

export default { empty, insert, remove };
