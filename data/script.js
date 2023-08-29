
// https://stackoverflow.com/questions/5999118/how-can-i-add-or-update-a-query-string-parameter
function setQueryParam(key, value) {
    var re = new RegExp("([?&])" + key + "=.*?(&|$)", "i");
    var uri = window.location.href;
    var separator = uri.indexOf('?') !== -1 ? "&" : "?";
    if (uri.match(re)) {
        window.location.href = uri.replace(re, '$1' + key + "=" + value + '$2');
    } else {
        window.location.href = uri + separator + key + "=" + value;
    }
}
