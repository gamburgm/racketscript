import { PrintablePrimitive } from './printable_primitive.js';
import { hashForEqual } from './hashing.js';
import * as UString from './unicode_string.js';

const WEAK_BOX_REPR = '#<weak-box>';
const WEAK_BOX_USTRING_REPR = UString.makeInternedImmutable(WEAK_BOX_REPR);

const primitiveLabel = Symbol('primitive');

class WeakBox extends PrintablePrimitive {
    constructor(v) {
        super();
        this.value = new WeakRef(WeakBox._canBeWeak(v) ? v : WeakBox._makePrimitiveKey(v));
    }

    get(maybeV) {
        const val = this.value.deref();
        // TODO is there a conventional way to check for a condition like this?
        if (val === undefined) return maybeV || false;
        return val;
    }

    equals(v) {
        return v === this;
    }

    hashForEqual() {
        return hashForEqual(this);
    }

    displayNativeString(out) {
        out.consume(WEAK_BOX_REPR);
    }

    displayUString(out) {
        out.consume(WEAK_BOX_USTRING_REPR);
    }

    writeNativeString(out) {
        this.displayNativeString(out);
    }

    writeUString(out) {
        this.displayUString(out);
    }

    static _canBeWeak(v) {
        return (v instanceof Object);
    }

    static _makePrimitiveKey(v) {
        return { primitiveLabel, key: v };
    }
}

export function make(v) {
    return new WeakBox(v);
}

export function check(v) {
    return (v instanceof WeakBox);
}
