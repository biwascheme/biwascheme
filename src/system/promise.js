import * as _ from "../deps/underscore-esm.js"
import { Bug } from "./error.js"

//
// R7RS Promise (lazy library; has nothing to do with JS Promise)
//
class BiwaPromise {
  constructor (done, thunk_or_value){
    this.box = [done, thunk_or_value];
  }

  // Return true when this promise is already calculated
  is_done() {
    return this.box[0];
  }

  // Return calculated value of this promise
  value() {
    if (!this.is_done()) {
      throw new Bug("this promise is not calculated yet");
    }
    return this.box[1];
  }

  thunk() {
    if (this.is_done()) {
      throw new Bug("this promise does not know the thunk");
    }
    return this.box[1];
  }

  update_with(new_promise) {
    this.box[0] = new_promise.box[0];
    this.box[1] = new_promise.box[1];
    new_promise.box = this.box;
  }
}

const isPromise = function(obj) {
  return (obj instanceof BiwaPromise);
};

// Create fresh promise
BiwaPromise.fresh = function(thunk) {
  return new BiwaPromise(false, thunk);
};
// Create calculated promise
BiwaPromise.done = function(value) {
  return new BiwaPromise(true, value);
};

export { BiwaPromise, isPromise };
