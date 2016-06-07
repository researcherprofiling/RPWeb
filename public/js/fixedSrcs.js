var finished = 0;
var target = 100; // Arbitrary value

searchButton = function (event) {
    event.preventDefault();
    finished = 0;
    target = 4;
    var name = document.getElementById("nameInput").value;
    var aff = document.getElementById("affiliationInput").value;
    IEEESearch(name, aff, function(data) {
        request({
            method: "POST",
            uri: window.location.href + "/update/IEEE",
            json: {
                "data": data,
                "search": {"name": name, "affiliation": aff}
            }
        }, function (err, res, body) {
            console.log("Updated IEEE");
            submitSearch();
        })
    }, function (err) {
        submitSearch();
    });
    scopus(name, aff, function(data) {
        request({
            method: "POST",
            uri: window.location.href + "/update/Scopus",
            json: {
                "data": data,
                "search": {"name": name, "affiliation": aff}
            }
        }, function (err, res, body) {
            console.log("Updated Scopus");
            submitSearch();
        })
    }, function (err) {
        submitSearch();
    });
    scienceDirect(name, aff, function(data) {
        request({
            method: "POST",
            uri: window.location.href + "/update/ScienceDirect",
            json: {
                "data": data,
                "search": {"name": name, "affiliation": aff}
            }
        }, function (err, res, body) {
            console.log("Updated Science Direct");
            submitSearch();
        })
    }, function (err) {
        submitSearch();
    });
    NSFSearch(name, aff, function(data) {
        request({
            method: "POST",
            uri: window.location.href + "/update/NSF",
            json: {
                "data": data,
                "search": {"name": name, "affiliation": aff}
            }
        }, function (err, res, body) {
            console.log("Updated NSF");
            submitSearch();
        })
    }, function (err) {
        submitSearch();
    });
};

var submitSearch = function() {
    finished += 1;
    if (finished == target) {
        document.getElementById("isSearch").value = true;
        document.getElementById("inputForm").submit();
    }
};

var refineButton = function() {
    event.preventDefault();
    document.getElementById("isSearch").value = true;
    document.getElementById("inputForm").submit();
};

document.getElementById('searchButton').addEventListener(
    'click', searchButton
);

document.getElementById('refineButton').addEventListener(
    'click', refineButton
)
