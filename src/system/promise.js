import Class from "./class.js"
import _ from "../deps/underscore-1.10.2-esm.js"

//
// R7RS Promise (lazy library)
//
const BiwaPromise = Class.create({
  initialize : function(done, thunk_or_value){
    this.box = [done, thunk_or_value];
  },

  // Return true when this promise is already calculated
  is_done: function() {
    return this.box[0];
  },

  // Return calculated value of this promise
  value: function() {
    if (!this.is_done()) {
      throw new BiwaScheme.Bug("this promise is not calculated yet");
    }
    return this.box[1];
  },

  thunk: function() {
    if (this.is_done()) {
      throw new BiwaScheme.Bug("this promise does not know the thunk");
    }
    return this.box[1];
  },

  update_with: function(new_promise) {
    this.box[0] = new_promise.box[0];
    this.box[1] = new_promise.box[1];
    new_promise.box = this.box;
  }
});

const isPromise = function(obj) {
  return (obj instanceof BiwaScheme.Promise);
};

// Create fresh promise
BiwaPromise.fresh = function(thunk) {
  return new BiwaScheme.Promise(false, thunk);
};
// Create calculated promise
BiwaPromise.done = function(value) {
  return new BiwaScheme.Promise(true, value);
};

export { BiwaPromise, isPromise };
