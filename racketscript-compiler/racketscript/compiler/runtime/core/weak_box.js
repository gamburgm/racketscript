import { PrintablePrimitive } from './printable_primitive.js';
import { hashForEqual } from './hashing.js';
import * as UString from './unicode_string.js';
import * as WeakUtils from './weak_utils.js';

const WEAK_BOX_REPR = '#<weak-box>';
const WEAK_BOX_USTRING_REPR = UString.makeInternedImmutable(WEAK_BOX_REPR);

class WeakBox extends PrintablePrimitive {
    constructor(v) {
        super();
        this.value = new WeakRef(WeakUtils.canBeWeak(v) ? v : WeakUtils.makePrimitiveKey(v));
    }

    get(maybeVal) {
        const val = this.value.deref();
        if (val === undefined) return maybeVal;
        if (val.primitiveLabel === WeakUtils.primitiveLabel) return val.key;
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
}

export function make(v) {
    return new WeakBox(v);
}

export function check(v) {
    return (v instanceof WeakBox);
}
