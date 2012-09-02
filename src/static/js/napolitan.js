$(function() {
  // enhance Date class prototype
  Date.prototype.toLocalDateInISO8601 = function() {
    var month = this.getMonth() + 1;
    var day = this.getDate();
    return this.getFullYear() + "-"
      + (month < 10 ? ("0" + month) : month) + "-"
      + (day < 10 ? ("0" + day) : day);
  }
});
