jinzamomi.krkr.global.String = (function(){
  var global = jinzamomi.krkr.global;
  var String = function(content) {
    if(!content) {
      this.content_ = "";
    } else {
      var jString = jinzamomi.krkr.global.String;
      if(!(from_ instanceof jString)) {
        throw new jinzamomi.krkr.global.Exception("argument is not array:" + from_);
      }
      this.content_ = content.content_;
    }
  };
  var jString = String;
  String.new_ = function(str) {
    var obj = new jString();
    obj.content_ = str;
    return obj;
  };
  String.prototype = {
  };
  return String;
})();
