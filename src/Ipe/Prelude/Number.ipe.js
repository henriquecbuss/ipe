// @ts-check

/**
 * @param {number} input
 * @returns {["Nothing"] | ["Just", string]}
 */
const toString = (input) => {
  const asString = input.toString();

  if (asString === undefined) {
    return ["Nothing"];
  }

  return ["Just", asString];
};

/**
 * @param {string} input
 * @returns {["Nothing"] | ["Just", number]}
 */
const fromString = (input) => {
  const asNumber = Number(input);

  if (isNaN(asNumber)) {
    return ["Nothing"];
  }

  return ["Just", asNumber];
};

export default { toString, fromString };
