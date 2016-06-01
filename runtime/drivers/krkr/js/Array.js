jinzamomi.krkr.global.Array = (function(){
  var global = jinzamomi.krkr.global;
  var Array = function(content) {
    if(!content) {
      this.content_ = [];
    } else {
      var jArray = jinzamomi.krkr.global.Array;
      if(!(from_ instanceof jArray)) {
        throw new jinzamomi.krkr.global.Exception("argument is not array:" + from_);
      }
      this.content_ = content.content_;
    }
  };
  Array.prototype = {
    get count() {
      return this.content_.length;
    },
    set count(len) {
      this.content_.length = len;
    },
    load: function(fname, mode) {
      mode = mode || '';
      console.error("TODO: Implement this");
      // たぶんサーバへ送ることになる
    },
    save: function(fname, mode) {
      mode = mode || '';
      console.error("TODO: Implement this");
    },
    saveStruct: function(fname, mode) {
      mode = mode || '';
      console.error("TODO: Implement this");
    },
    pack: function(template) {
      console.error("TODO: Implement this");
    },
    split: function(delims, str, reserverd, ignoreEmpty) {
      ignoreEmpty = ignoreEmpty || false;
      if(typeof delims === 'string' && delims.length > 1) {
        delims = Regexp('['+delims.split('').map(function(s) { return '\\'+s; }).join('')+']');
      }
      var splitted = str.split(delims);
      if (ignoreEmpty) {
        splitted = splitted.filter(function (s) { return s.length !== 0 });
      }
      this.content_ = splitted;
      return this;
    },
    join: function(delim, reserverd, ignoreEmpty) {
      ignoreEmpty = ignoreEmpty || false;
      if(ignoreEmpty) {
        return this.content_.filter(function (s) { return s.length !== 0 }).join(delim);
      } else {
        return this.content_.join(delim);
      }
    },
    reverse: function() {
      this.content_.reverse();
      return this;
    },
    assign: function(from_) {
      var jArray = jinzamomi.krkr.global.Array;
      if(!(from_ instanceof jArray)) {
        throw new jinzamomi.krkr.global.Exception("argument is not array:" + from_);
      }
      this.content_ = from.content_.concat();
      return this;
    },
    assignStruct: function(from_) {
      var jDictionary = jinzamomi.krkr.global.Dictionary;
      var jArray = jinzamomi.krkr.global.Array;
      if(!(from_ instanceof jArray)) {
        throw new jinzamomi.krkr.global.Exception("argument is not array:" + from_);
      }
      var fc = from_.content_;
      var nc = [];
      for(var i = 0; i < fc.length; i++) {
        if(fc[i] instanceof jDictionary) {
          var n = new jDictionary();
          n.assignStruct(fc[i]);
          nc.push(n);
        }else if(fc[i] instanceof jArray) {
          var n = new jArray();
          n.assignStruct(fc[i]);
          nc.push(n);
        }else{
          nc.push(fc[i]);
        }
      }
      this.content_ = from.content_.concat();
      return this;
    },
    clear: function() {
      this.content_ = [];
      return this;
    },
    erase: function(idx) {
      this.content_.splice(idx, 1);
      return this;
    },
    remove: function(v, all) {
      all = all !== undefined ? all : true;
      var erased = false;
      this.content_ = this.content_.filter(function(t) {
        var er = v !== t || (!all && erased);
        erased = erased || !er;
        return er;
      });
      return this;
    },
    insert: function(idx, v) {
      this.content_.splice(idx, 0, v);
      return this;
    },
    add: function(v) {
      this.content_.push(v);
      return this.content_.length-1;
    },
    push: function() {
      this.content_.push.apply(this.content_, arguments);
      return this.content_.length;
    },
    unshift: function() {
      this.content_.unshift.apply(this.content_, arguments);
      return this.content_.length;
    },
    find: function(v) {
      return this.content_.indexOf(v);
    },
    pop: function() {
      return this.content_.pop();
    },
    shift: function() {
      return this.content_.shift();
    }
  };
  return Array;
})();
