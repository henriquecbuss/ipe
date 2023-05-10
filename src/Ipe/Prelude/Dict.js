/**
 * @template T
 * @returns {Map<string, T>}
 */
const empty = () => {
  return new Map();
};
/**
 * @template T
 * @param {string} key
 * @param {T} value
 * @param {Map<string, T>} dict
 * @returns {Map<string, T>}
 */
const insert = (key, value, dict) => {
  return dict.set(key, value);
};

/**
 * @template T
 * @param {string} key
 * @param {Map<string, T>} dict
 * @returns {Map<string, T>}
 * */
const remove = (key, dict) => {
  dict.delete(key);
  return dict;
};

export default { empty, insert, remove };
