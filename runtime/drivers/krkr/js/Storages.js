jinzamomi.krkr.global.Storages = (function() {
  var global = jinzamomi.krkr.global;
  var paths = [];
  var Storages = {
    /**
     * 自動検索パスへの追加
     * @param {string} path
     */
    addAutoPath: function(path) {
      var listPath = global.System.projectBase_ + path + "/files-list.json";
      global.GetJSON(listPath, function(files) {
        paths.push({path: path, files: files});
        console.log(paths);
      });
    },
    /**
     * ストレージの存在確認
     * @param {string} path
     * @return {boolean}
     */
    isExistentStorage: function(storage) {
      return false;
    }
  };
  return Storages;
})();
