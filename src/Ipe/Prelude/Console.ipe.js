/**
 * @param {string} message
 * @returns {string} the original message
 */
const log = (message) => {
  console.log(message);
  return message;
};

/**
 * @template T
 * @param {string} message
 * @param {T} value
 * @returns {T} the original value
 */
const andLog = (message, value) => {
  console.log(message);
  return value;
};

/**
 * @template T
 * @param {(value: T) => string} toString
 * @param {T} value
 * @returns {T} the original value
 */
const andLogValue = (toString, value) => {
  console.log(toString(value));
  return value;
};
