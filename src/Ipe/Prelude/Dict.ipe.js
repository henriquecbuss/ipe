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

/**
 * @template Key
 * @template Value
 * @template Result
 * @param {(key: Key) => (value: Value) => Result} transformer
 * @returns {(inputDict: Map<Key, Value>) => Map<Key, Result>}
 */
const map = (transformer) => (inputDict) => {
  const result = new Map();
  for (const [key, value] of inputDict) {
    result.set(key, transformer(key)(value));
  }
  return result;
};

/**
 * @template Key
 * @template Value
 * @param {Map<Key, Value>} dict
 * @returns {Array<{key: Key, value: Value}>}
 */
const toList = (dict) => {
  const result = [];
  for (const [key, value] of dict) {
    result.push({ key, value });
  }
  return result;
};

/**
 * @template Key
 * @template Value
 * @param {Key} key
 * @returns {(dict: Map<Key, Value>) => ['Just', Value] | ['Nothing']}
 */
const get = (key) => (dict) => {
  const item = dict.get(key);
  if (item === undefined) {
    return ["Nothing"];
  }

  return ["Just", item];
};

export default { empty, insert, remove, map, toList, get };
