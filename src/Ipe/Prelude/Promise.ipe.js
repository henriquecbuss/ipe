/**
 * @template TOriginal
 * @template TTransformed
 * @param {Promise<TOriginal>} promise
 * @returns {((original: TOriginal) => Promise<TTransformed>) => * Promise<TTransformed>}
 */
const andThen = (promise) => (transform) => {
  return promise.then(transform);
};

/**
 * @template TOriginal
 * @template TTransformed
 * @param {Promise<TOriginal>} promise
 * @returns {((original: TOriginal) => TTransformed) => * Promise<TTransformed>}
 */
const map = (promise) => (transform) => {
  return promise.then(transform);
};

/**
 * @template T
 * @param {T} value
 * @returns {Promise<T>}
 */
const succeed = (value) => Promise.resolve(value);

export default { andThen, map, succeed };
