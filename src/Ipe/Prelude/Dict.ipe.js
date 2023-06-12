// @ts-check

/**
 * @template Key
 * @template Value
 * @argument {Record<never, never>} _
 * @returns {Map<Key, Value>}
 */
const empty = (_) => {
  return new Map();
};

/**
 * @template Key
 * @template Value
 * @param {Key} key
 * @returns {(value: Value) => (dict: Map<Key, Value>) => Map<Key, Value>}
 */
const insert = (key) => (value) => (dict) => {
  dict.set(key, value);
  return dict;
};

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
