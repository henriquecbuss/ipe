// @ts-check

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
 * @returns {(value: T) => T}
 */
const andLog = (message) => (value) => {
  console.log(message);
  return value;
};

/**
 * @template T
 * @param {(value: T) => string} toString
 * @returns {(value: T) => T}
 */
const andLogValue = (toString) => (value) => {
  console.log(toString(value));
  return value;
};

export default { log, andLog, andLogValue };
