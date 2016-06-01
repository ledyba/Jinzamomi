jinzamomi.krkr.global.Array = (function(){
  var global = jinzamomi.krkr.global;
  var Dictionary = function(content) {
    if(!content) {
      this.content_ = {};
    } else {
      var jDictionary = jinzamomi.krkr.global.Dictionary;
      if(!(from_ instanceof jDictionary)) {
        throw new jinzamomi.krkr.global.Exception("argument is not dict:" + from_);
      }
      this.content_ = content.content_;
    }
  };
  Dictionary.prototype = {
    saveStruct: function(fname, mode) {
      mode = mode || '';
      console.error("TODO: Implement this");
    },
    assign: function(obj, clear) {
      clear = clear !== undefined ? clear : true;
      var jDictionary = jinzamomi.krkr.global.Dictionary;
      if(!(obj instanceof jDictionary)) {
        throw new jinzamomi.krkr.global.Exception("argument is not dict:" + obj);
      }
      var c = clear ? {} : this.content_;
      for(key in obj.contents_) {
        if (obj.contents_.hasOwnProperty(key)) {
          c[key] = obj.contents_[key];
        }
      }
      this.content_ = c;
    },
    assignStruct: function(obj) {
      var jDictionary = jinzamomi.krkr.global.Dictionary;
      if(!(obj instanceof jDictionary)) {
        throw new jinzamomi.krkr.global.Exception("argument is not dict:" + obj);
      }
      // clear all contents first.
      // https://github.com/krkrz/krkrz/blob/master/src/core/tjs2/tjsDictionary.cpp#L543
      var c = {};
      for(key in obj.contents_) {
        if (obj.contents_.hasOwnProperty(key)) {
          var v = obj.contents_[key];
          if(v instanceof jDictionary) {
            var n = new jDictionary();
            n.assignStruct(v);
            c[key] = n;
          }else if(v instanceof jArray) {
            var n = new jArray();
            n.assignStruct(v);
            c[key] = n;
          }else{
            c[key] = v;
          }
        }
      }
      this.content_ = c;
    },
    clear: function() {
      this.content_ = {};
    }
  };
  return Dictionary;
})();
