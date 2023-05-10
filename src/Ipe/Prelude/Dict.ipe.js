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
 * @param {Value} value
 * @param {Map<Key, Value>} dict
 * @returns {Map<Key, Value>}
 */
const insert = (key, value, dict) => {
  return dict.set(key, value);
};

/**
 * @template Key
 * @template Value
 * @param {Value} key
 * @param {Map<Value, Key>} dict
 * @returns {Map<Value, Key>}
 * */
const remove = (key, dict) => {
  dict.delete(key);
  return dict;
};

export default { empty, insert, remove };
