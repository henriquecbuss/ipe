// @ts-check

/**
 * @template A
 * @template B
 * @argument {(input: A) => B} transformer
 * @returns {(inputList: Array<A>) => Array<B>}
 */
const map = (transformer) => (inputList) => {
  return inputList.map(transformer);
};

export default { map };
