export function mapAsync<T, U>(
  arr: T[],
  fn: (item: T) => Promise<U>
): Promise<U[]> {
  return Promise.all(arr.map(fn));
}
