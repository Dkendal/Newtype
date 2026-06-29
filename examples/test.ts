type Assert<T extends true> = T;
type Refute<T extends false> = T;

type Equals<A, B> =
  (<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2)
  ? true : false;

type Extends<A, B> = [A] extends [B] ? true : false;

type _00 = Assert<Extends<string, unknown>>;
type _01 = Assert<Extends<string, any>>;
type _02 = Refute<Extends<string, never>>;
type _03 = Assert<Extends<string, {}>>;
type _04 = Assert<Extends<string, Object>>;

type _10 = Assert<Extends<number, unknown>>;
type _11 = Assert<Extends<number, any>>;
type _12 = Refute<Extends<number, never>>;
type _13 = Assert<Extends<number, {}>>;
type _14 = Assert<Extends<number, Object>>;

type _20 = Assert<Extends<boolean, unknown>>;
type _21 = Assert<Extends<boolean, any>>;
type _22 = Refute<Extends<boolean, never>>;
type _23 = Assert<Extends<boolean, {}>>;
type _24 = Assert<Extends<boolean, Object>>;

type _30 = Assert<Extends<{}, unknown>>;
type _31 = Assert<Extends<{}, any>>;
type _32 = Assert<Extends<{}, {}>>;
type _33 = Assert<Extends<{ x: string }, { x: string }>>;
type _34 = Assert<Extends<{ x: 1, y: 2 }, { x: 1 }>>;
type _35 = Assert<Extends<{ x: '' }, { x: string }>>;

type _40 = Assert<Extends<() => any, () => any>>;
type _41 = Assert<Extends<() => never, () => any>>;
type _42 = Assert<Extends<() => unknown, () => any>>;
type _43 = Assert<Extends<() => any, Function>>;
type _44 = Assert<Extends<(x: any) => any, Function>>;
type _45 = Assert<Extends<() => any, (x: any) => any>>;
type _46 = Assert<Extends<(x: any) => any, (x: any, y: any) => any>>;
