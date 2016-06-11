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
  global.__defineKlass = function(name, proto, exts){
    var cstr = proto[name];
    var f = function() {
      cstr.apply(this, arguments);
    };
    f.prototype = proto;
    return f;
  };

  return krkr;
})();
