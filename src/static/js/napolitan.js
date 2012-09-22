$(function() {
  Date.prototype.toLocalDateInISO8601 = function() {
    var month = this.getMonth() + 1;
    var day = this.getDate();
    return this.getFullYear() + "-"
      + (month < 10 ? ("0" + month) : month) + "-"
      + (day < 10 ? ("0" + day) : day);
  };

  Date.prototype.getDaySinceEpoch = function() {
    var now = Date.UTC(this.getFullYear(), this.getMonth(), this.getDay());
    var epoch = Date.UTC(1970, 0, 1);
    console.log('now = ' + now + ' / epoch = ' + epoch + " / y = " + this.getFullYear() + " / m = " + this.getMonth() + " / d = " + this.getDay());
    return Math.ceil((now - epoch) / (24 * 60 * 60 * 1000.0));
  };

  Date.prototype.getWeekSinceEpoch = function() {
    var dse = this.getDaySinceEpoch();
    return Math.ceil((dse + 3) / 7.0);
  };

});
