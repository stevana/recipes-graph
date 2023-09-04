function makeButtonsSetQueryParams() {
    function setQueryParam(key, value) {
        if (!key || !value) {
            return;
        }
        const params = new URLSearchParams(window.location.search);
        if (params.get(key) == value) {
            params.delete(key);
        } else {
            params.set(key, value);
        }
        params.sort();
        window.location.search = params.toString();
    }
    const btns = document.querySelectorAll('button');
    btns.forEach(btn => {
        btn.addEventListener('click', event => {
            setQueryParam(event.target.classList[0], event.target.id);
        });
    });
}

function highlightSelectedMenuItems() {
    const params = new URLSearchParams(window.location.search);
    for (const [key, value] of params) {
        const btn = document.querySelector("." + key + "#" + value);
        if (btn) {
            // btn.style.textDecoration  = 'underline';
            btn.style.backgroundColor = 'lightgray';
        }
    }
}

function changeUrlBasedOnQueryParams() {
    const params = new URLSearchParams(window.location.search);
    var keys = "";
    for (const key of params.keys()) {
        if (keys == "") {
            keys += key
        } else {
            keys += "-" + key
        }
    }

    var values = "";
    var length = 0;
    for (const value of params.values()) {
        length++;
        if (values == "") {
            values += value
        } else {
            values += "-" + value
        }
    }

    const pathnames = window.location.pathname.split('/');
    var newPage = keys == "" ? "index.html" : keys + "-" + values + ".html";
    if (pathnames[pathnames.length - 1] == newPage) {
        return
    } else {
        pathnames[pathnames.length - 1] = newPage;
        window.location.pathname = pathnames.join('/');
    }
}

makeButtonsSetQueryParams();
highlightSelectedMenuItems();
changeUrlBasedOnQueryParams();
