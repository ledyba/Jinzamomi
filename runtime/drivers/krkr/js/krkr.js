jinzamomi.krkr = (function() {
  var krkr = {};
  var global = {};
  krkr.global = global;
  global.global = global;
  /**
   * main function
   * @param {string} path - project path
   * @param {Object.<string, string>} args
   */
  krkr.main = function(path, args) {
    global.System.projectBase_ = path;
    global.System.args_ = args;
    global.Storages.addAutoPath("");
    global.Scripts.execStorage("startup.tjs");
  };
  global.Window = {};
  /**
   * getPropertyDescriptor
   * @param {Object} obj
   * @param {string} name
   * @return {Object}
   */
  krkr.getPropertyDescriptor = function(obj, name) {
    if(!(name in obj)){
      throw Error(name + " not found in "+obj);
    }
    for(;!obj.hasOwnProperty(name);obj = obj.__proto__);
    return Object.getOwnPropertyDescriptor(obj, name);
  };
  global.GetJSON = function(url, clbk, err) {
    var r = new XMLHttpRequest();
    r.onreadystatechange = function() {
      // readyState = 4(complete)
      // status = httpresponseのこと
      if(r.readyState == 4) {
        if(r.status == 200) {
          clbk(JSON.parse( r.responseText ));
        }else{
          if(err) {
            err(r.status, r.responseText);
          } else {
            console.error(r.status, r.responseText);
          }
        }
      }
    };
    r.open("GET", url, false);
    r.send("");
  };
  global.Eval = function(url) {
    var r = new XMLHttpRequest();
    r.onreadystatechange = function() {
      // readyState = 4(complete)
      // status = httpresponseのこと
      if(r.readyState == 4) {
        (0 || eval)(r.responseText);
      }
    };
    r.open("GET", url, false);
    r.send("");
  };
  global.__defineKlass = function(name, proto_, exts){
    var cstr = proto_[name];
    var f = function() {
      cstr.apply(this, arguments);
    };
    if(exts.length > 0) {
      var proto = {};
      for(var i=0;i < exts.length; i++) {
        var super_ = global[exts[i]];
        if(!super_) {
          console.error("Class not found:", exts[i], "extended by",name);
          return null;
        }
        Object.assign(proto, super_.prototype);
      }
      Object.assign(proto, proto_);
      if(exts.length === 1) {
        var superName = exts[0];
        var super_ = global[superName];
        if(!super_) {
          console.error("Class not found:", exts[i], "extended by",name);
          return null;
        }
        proto["__super"] = super_.prototype;
      }
      f.prototype = proto;
    } else {
      f.prototype = proto_;
    }
    return f;
  };

  return krkr;
})();
