/**
 * @typedef {import("../Json").JsonValue} JsonValue
 */

/**
 *
 * @param {string} input The string to encode
 * @returns {string} The encoded string
 */
const string = (input) => input;

/**
 *
 * @param {boolean} input The boolean to encode
 * @returns {boolean} The encoded boolean
 */
const bool = (input) => input;

/**
 *
 * @param {number} input The number to encode
 * @returns {number} The encoded number
 */
const number = (input) => input;

/**
 *
 * @returns {null} The encoded null
 */
const nothing = () => null;

/**
 * @template T
 * @param {(input: T) => JsonValue} encodeItem
 * @param {T[]} arr
 * @returns {JsonValue}
 */
const list = (encodeItem, arr) => arr.map(encodeItem);

/**
 * @param {Map<string, JsonValue>} input
 * @returns {JsonValue}
 */
const object = (input) => {
  const result = {};
  for (const [key, value] of input) {
    result[key] = value;
  }
  return result;
};

export default {
  string,
  bool,
  number,
  nothing,
  list,
  object,
};
