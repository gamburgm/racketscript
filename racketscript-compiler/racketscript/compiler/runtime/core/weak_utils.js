export const primitiveLabel = Symbol('primitive');

export function canBeWeak(v) {
    return (v instanceof Object);
}

export function makePrimitiveKey(v) {
    return { primitiveLabel, key: v };
}
