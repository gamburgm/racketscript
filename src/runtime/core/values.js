import {default as Primitive} from "./primitive.js";
import {RacketCoreError} from "./error.js";

class Values extends Primitive {
    constructor(vals) {
	super();
	this.v = vals;
    }

    toString() {
	throw new RacketCoreError("Not Implemented");
    }

    toRawString() {
	return this.toString();
    }

    getAt(i) {
	return this.v[i];
    }

    getAll() {
	return this.v;
    }
}


export
function make(vals) {
    return new Values(vals);
}

export
function check(v) {
    return (v instanceof Values);
}
