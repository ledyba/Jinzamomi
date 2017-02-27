describe('ArraySuite', () => {
  var jArray = jinzamomi.krkr.global.Array;
  var jString = jinzamomi.krkr.global.String;
  it('count test', () => {
    var arr = new jArray();
    expect(arr).to.not.be(null);
    expect(arr).to.be.a(jArray);
    expect(arr.count).to.eql(0);

    arr.add(jString.new_("a"));
    expect(arr.count).to.eql(1);
  });
});
