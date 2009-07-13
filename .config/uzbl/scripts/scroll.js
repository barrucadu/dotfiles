// VIM ruler style scroll message
(function() {
    var run = Uzbl.run;
    var updateMsg = function() {
        var innerHeight = window.innerHeight;
        var scrollY = window.scrollY;
        var height = document.height;

        if (! document.height) {
            run("set status_message=");
        }
        else if (height <= innerHeight) {
            run("set status_message=All");
        }
        else if (scrollY === 0) {
            run("set status_message=Top");
        }
        else if (scrollY + innerHeight === height) {
            run("set status_message=Bot");
        }
        else {
            var percentage = Math.round(scrollY / (height - innerHeight) * 100);
            run("set status_message=" + percentage + "%");
        }
    };

    if (! document.body) {
        window.addEventListener("DOMContentLoaded", updateMsg, false);
        window.addEventListener("load", updateMsg, false);
        window.addEventListener("resize", updateMsg, false);
        window.addEventListener("scroll", updateMsg, false);
    }

    updateMsg();
})();
