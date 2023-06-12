// @ts-check

/**
 * @typedef {import("../Json.ipe").JsonValue} JsonValue
 */

/**
 * @template T
 * @typedef {["Ok", T] | ["Err", string] } ParseResult
 */

/**
 * @template T
 * @typedef {(input: JsonValue) => ParseResult<T>} Parser
 */

/**
 * @template T
 * @param {Parser<T>} parser
 * @returns {(input: JsonValue) => ParseResult<T>}
 */
const parseJson = (parser) => (input) => {
  return parser(input);
};

/**
 * @template T
 * @param {Parser<T>} parser
 * @returns {(input: string) => ParseResult<T>}
 */
const parseString = (parser) => (input) => {
  try {
    /** @type {JsonValue} */
    const parsed = JSON.parse(input);
    return parseJson(parser)(parsed);
  } catch (error) {
    return ["Err", `Invalid JSON: ${error}`];
  }
};

/**
 * @template T
 * @param {T} value
 * @returns {Parser<T>}
 */
const succeed = (value) => {
  return (_) => ["Ok", value];
};

/**
 * @template A
 * @template B
 * @template C
 * @param {(a: A) => (b: B) => C} transformer
 * @returns {(a: Parser<A>) => (b: Parser<B>) => Parser<C>}
 */
const map2 = (transformer) => (a) => (b) => {
  return (input) => {
    const aResult = a(input);
    if (aResult[0] === "Err") {
      return aResult;
    }

    const bResult = b(input);
    if (bResult[0] === "Err") {
      return bResult;
    }

    const result = transformer(aResult[1])(bResult[1]);

    return ["Ok", result];
  };
};

/**
 * @template A
 * @param {string} fieldName
 * @returns {(parser: Parser<A>) => Parser<A>}
 */
const field = (fieldName) => (parser) => {
  return (input) => {
    if (
      typeof input !== "object" ||
      input === null ||
      Array.isArray(input) ||
      input === undefined
    ) {
      return ["Err", `Expected object, but got ${typeof input}`];
    }

    if (!(fieldName in input)) {
      return ["Err", `Expected field ${fieldName}`];
    }

    // @ts-expect-error
    return parser(input[fieldName]);
  };
};

/**
 * @type {Parser<string>}
 */
const string = (input) => {
  if (typeof input === "string") {
    return ["Ok", input];
  }

  return ["Err", `Expected string, but got ${typeof input}`];
};

/**
 * @type {Parser<number>}
 */
const number = (input) => {
  if (typeof input === "number") {
    return ["Ok", input];
  }

  return ["Err", `Expected number, but got ${typeof input}`];
};

/**
 * @template T
 * @param {Parser<T>} itemParser
 * @returns {Parser<T[]>}
 */
const list = (itemParser) => {
  return (input) => {
    if (!Array.isArray(input)) {
      return ["Err", `Expected list, but got ${typeof input}`];
    }

    /** @type {T[]} */
    const result = [];
    for (const item of input) {
      const itemResult = itemParser(item);

      if (itemResult[0] === "Err") {
        return itemResult;
      }

      result.push(itemResult[1]);
    }

    return ["Ok", result];
  };
};

export default {
  parseJson,
  parseString,
  succeed,
  map2,
  field,
  string,
  number,
  list,
};
