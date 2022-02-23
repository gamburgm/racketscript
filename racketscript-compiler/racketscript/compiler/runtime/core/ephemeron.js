import { PrintablePrimitive } from './printable_primitive.js';
import { hashForEqual } from './hashing.js';
import * as UString from './unicode_string.js';
import * as WeakUtils from './weak_utils.js';

const EPHEMERON_REPR = '#<ephemeron>';
const EPHEMERON_USTRING_REPR = UString.makeInternedImmutable(EPHEMERON_REPR);

class Ephemeron extends PrintablePrimitive {
    // FIXME do I need this roundabout technique?
    static _registry = new FinalizationRegistry((killEphemeronValue) => {
        killEphemeronValue();
    });

    _key;
    _val;

    constructor(key, v) {
        super();
        this._key = new WeakRef(WeakUtils.canBeWeak(key) ? key : WeakUtils.makePrimitiveKey(key));
        this._val = v;
        Ephemeron._registry.register(this._key, () => { this._killValue(); });
    }

    get(maybeVal) {
        return this._val || maybeVal;
    }

    equals(v) {
        return v === this;
    }

    hashForEqual() {
        return hashForEqual(this);
    }

    displayNativeString(out) {
        out.consume(EPHEMERON_REPR);
    }

    displayUString(out) {
        out.consume(EPHEMERON_USTRING_REPR);
    }

    writeNativeString(out) {
        this.displayNativeString(out);
    }

    writeUString(out) {
        this.displayUString(out);
    }

    _killValue() {
        this._val = undefined;
    }
}

export function make(key, v) {
    return new Ephemeron(key, v);
}

export function check(v) {
    return (v instanceof Ephemeron);
}
