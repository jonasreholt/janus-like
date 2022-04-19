$(function(){
    editor = ace.edit("editor");
    editor.setTheme("ace/theme/tomorrow");
    editor.setShowPrintMargin(false);
    editor.getSession().setMode("ace/mode/janus");
    editor.getSession().setTabSize(4);
    editor.getSession().setUseSoftTabs(true);
    editor.commands.addCommand({
        name: 'runCommand',
        bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
        exec: function(editor) {
            execute();
        },
        readOnly: true // false if this command should not apply in readOnly mode
    });

    var $editor = $("#editor");
    var $outputPane = $("#output-pane");
    var $output = $("#output");
    var $executing = $("#output-pane .executing");

    function setEditorContent(code) {
        editor.setValue(code);
        editor.gotoLine(0);
        removeErrorMarkers();
    }

    function loadCode(hash) {
        var match = hash.match(/#examples\/([a-zA-Z0-9-]+)/);
        if (match) {
            $.get("examples/" + match[1] + ".japa").done(setEditorContent);
            return;
        }

        match = hash.match(/#([a-z0-9]{8})/);
        if (match) {
            $.get("load.php", {hash: match[1]}).done(setEditorContent);
        }
    }

    function execute(options) {
        defaults = {
            "outputHeight": 200
        }
        if (typeof options == "object") {
            options = $.extend(defaults, options);
        } else {
            options = defaults;
        }

        $executing.fadeIn();
        var code = editor.getValue();
        $.post("execute.php", {
            "code": code
        })
            .done(formatOutput)
            .fail(formatError)
            .always(function() {
                $executing.fadeOut();
            });
    }

    var prevErrors = [];
    function removeErrorMarkers() {
        var session = editor.getSession();
        for (var i = 0; i < prevErrors.length; ++i) {
            session.removeGutterDecoration(prevErrors[i], "errorGutter");
            prevErrors = [];
        }
    }


    function formatOutput(output) {
        // First line contains the exit code
        var retval = parseInt(output.substr(0, output.indexOf("\n")));
        output = output.substring(output.indexOf("\n") + 1);

        removeErrorMarkers();
        if (retval > 0) {
            var session = editor.getSession();
            var match = output.match(/line (\d+), column (\d+)/);

            if (match) {
                var line = parseInt(match[1]) - 1;
                session.addGutterDecoration(line, "errorGutter");
                prevErrors.push(line);
            }
            $output.html($("<pre>").text(output).addClass("error"));
        } else {
            $output.html($("<pre>").text(output));
        }
    }

    function formatError(data) {
        $output.html(
            '<div class="alert alert-error">An error occured while trying to run the program.</div>'
        );
    }

    $("#examples a").click(function(e) {
        loadCode(e.target.hash);
    });

    $("#Cpp").click(execute);

    $(window).on("hashchange", function() { loadCode(location.hash); });
    $(window).on("beforeunload", function() {
        return "All your changes will be lost when you leave the site.";
    });

    loadCode(location.hash);
});
