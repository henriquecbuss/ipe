// @ts-check

/**
 * @typedef {import("../Json.ipe").JsonValue} JsonValue
 */

/**
 * @template T
 * @typedef {{ ok: true; parsed: T } | { ok: false; error: string }} ParseResult
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
    return { ok: false, error: `Invalid JSON: ${error}` };
  }
};

/**
 * @template T
 * @param {T} value
 * @returns {Parser<T>}
 */
const succeed = (value) => {
  return (_) => ({ ok: true, parsed: value });
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
    if (!aResult.ok) {
      return aResult;
    }

    const bResult = b(input);
    if (!bResult.ok) {
      return bResult;
    }

    const result = transformer(aResult.parsed)(bResult.parsed);

    return { ok: true, parsed: result };
  };
};

/**
 * @type {Parser<string>}
 */
const string = (input) => {
  if (typeof input === "string") {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected string, but got ${typeof input}` };
};

/**
 * @type {Parser<number>}
 */
const number = (input) => {
  if (typeof input === "number") {
    return { ok: true, parsed: input };
  }

  return { ok: false, error: `Expected number, but got ${typeof input}` };
};

/**
 * @template T
 * @param {Parser<T>} itemParser
 * @returns {Parser<T[]>}
 */
const list = (itemParser) => {
  return (input) => {
    if (!Array.isArray(input)) {
      return { ok: false, error: `Expected list, but got ${typeof input}` };
    }

    /** @type {T[]} */
    const result = [];
    for (const item of input) {
      const itemResult = itemParser(item);

      if (!itemResult.ok) {
        return itemResult;
      }

      result.push(itemResult.parsed);
    }

    return { ok: true, parsed: result };
  };
};

export default {
  parseJson,
  parseString,
  succeed,
  map2,
  string,
  number,
  list,
};
