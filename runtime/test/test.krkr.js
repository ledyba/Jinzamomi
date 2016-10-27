describe('ArraySuite', () => {
  var jArray = jinzamomi.krkr.global.Array;
  it('create test', () => {
    var arr = new jArray();
    expect(arr).to.not.be(null);
    expect(arr).to.be.a(jArray);
    expect(arr.count).to.eql(0);
  });
});
