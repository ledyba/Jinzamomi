jinzamomi.krkr.global.Exception = (function(){
  var global = jinzamomi.krkr.global;
  var Exception = function(message) {
    this.message = message;
  };
  Exception.prototype = Object.create(Error.prototype);
  return Exception;
})();
