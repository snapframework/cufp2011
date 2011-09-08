(function( $ ){
    var loginHTML = '<div class="chatwindow loginwindow">\
        <h1><img src="img/logo-small.png"/>\
            <span>Welcome to Snap Chat!</span>\
        </h1>\
        <p class="hello">\
        <a href="https://github.com/snapframework/cufp2011">Snap Chat</a> is\
        a tutorial application demonstrating long-polling JSON calls using\
        the <a href="http://snapframework.com/">Snap Framework</a>. To begin\
        chatting, please enter your desired username.</p>\
        <p class="error" style="display:none"></p>\
        <div class="form">\
          <form>\
            <label for="username">User name:</label>\
            <input maxlength="40" name="username" class="username" \
                   type="text"></input>\
            <button type="button" class="cupid-green">Join the chat room</button>\
          </form>\
        </div>\
        </div>';

    var chatRoomHTML = '<div class="chatwindow chatroom">\
          <div class="chatroom-top">\
            <form>\
              <button type="button" class="logoutButton cupid-green">⇦ Leave chat room</button>\
            </form>\
            <div class="top-header"><img src="img/logo-small.png"/> \
            <span class="top-message"></span></div>\
          </div>\
          <div class="chatroom-buffer"><div class="buffer">\
              <div class="chattext"></div>\
          </div></div>\
          <div class="chatroom-input">\
            <form>\
              <table><tr><td class="inputcell" width="*">\
              <input type="text" maxlength="1024" placeholder="Enter your message here" class="chatinput">\
              </input></td><td class="buttoncell" style="width: 5em">\
              <button type="button" class="write_message cupid-green">Send</button></td></tr></table>\
            </form>\
          </div>\
        </div>';

    var scrollToBottom = function(dataObj) {
        var chatDiv = dataObj['chatDiv'];
        var $buffer = $('.buffer', chatDiv);
        var ov = $buffer.css('overflow');
        if (ov == 'auto') {
            /* we're on a normal computer */
            $b = $buffer;
            threshold = 10;
            oh = $buffer.outerHeight();
        } else {
            /* we're on a handheld here. */
            $b = $('body');
            threshold = 30;
            oh = $(window).height();
        }

        $b.scrollTop($b.prop('scrollHeight') - oh);
    }

    var addMessageToBuffer = function(dataObj, message) {
        var chatDiv = dataObj['chatDiv'];
        var $buffer = $('.buffer', chatDiv);
        /* Are we at the bottom already? */
        var atBottom = false;
        var ov = $buffer.css('overflow');
        var $b, threshold, oh;

        if (ov == 'auto') {
            /* we're on a normal computer */
            $b = $buffer;
            threshold = 10;
            oh = $buffer.outerHeight();
        } else {
            /* we're on a handheld here. */
            $b = $('body');
            threshold = 30;
            oh = $(window).height();
        }


        if ($b.prop('scrollTop') + oh >=
            $b.prop('scrollHeight') - threshold) {
            atBottom = true;
        }

        var $text = $('.chattext', chatDiv);
        $text.append(message);

        if (atBottom) {
            setTimeout(function() {
                scrollToBottom(dataObj);
            }, 1);
        }

        $(message).effect("highlight", {}, 3000);
        dataObj.numMessages = dataObj.numMessages + 1;

        // limit browser memory usage.
        if (dataObj.numMessages > 200) {
            $('.message:first-child').remove();
        }
    }

    var withZero = function(t) {
        /* javascript, y u no have sprintf()? */
        if (t >= 0 && t < 10) {
            return '0' + t.toString();
        } else {
            return t.toString();
        }
    }

    var fmtTime = function(posixtime) {
        var dt = new Date(parseInt(posixtime) * 1000);
        var y  = dt.getFullYear();
        var mo = withZero(dt.getMonth());
        var d  = withZero(dt.getDate());
        var h  = withZero(dt.getHours());
        var m  = withZero(dt.getMinutes());
        var s  = withZero(dt.getSeconds());

        return ('[' + y + '/' + mo + '/' + d + ' ' + h + ':' + m + ':' + s
                + ']');
    }

    var cleanupOutstandingRequests = function(dataObj) {
        dataObj['suppress_error'] = true;

        if (dataObj['fetcher']) {
            dataObj['fetcher'].abort();
        }
    }

    var mkMsg = function(posixtime, user, message, extraClass) {
        var cls = "message";
        if (extraClass) { cls = cls + " " + extraClass; }
        var p = $('<p class="' + cls + '"></p>');
        var t = $('<span class="time"></span>');
        var u = $('<span class="username"></span>');
        var m = $('<span class="messageText"></span>');
        $(t).text(fmtTime(posixtime));
        $(u).text(user);
        $(m).text(message);
        $(p).append(t).append(u).append(m);
        return p;
    }

    var gotMessage = function(dataObj, msg) {
        var msgs = msg.messages;
        for (var i in msgs) {
            var src = msgs[i];
            var type = src.contents.type;
            var msg;
            if (type == 'join') {
                msg = mkMsg(src.time, src.user, "has joined the channel.",
                           'meta');
            } else if (type == 'leave') {
                msg = mkMsg(src.time, src.user, src.contents.text, 'meta');
            } else if (type == 'talk') {
                msg = mkMsg(src.time, '<' + src.user + '>', src.contents.text);
            } else {
                msg = mkMsg(src.time, src.user, src.contents.text);
            }

            addMessageToBuffer(dataObj, msg);
        }

        fetchMessages(dataObj);
        return this;
    }

    var fetchError = function(dataObj, obj, reason) {
        if (dataObj.suppress_error) return;
        var now = (new Date()).valueOf() / 1000;
        var msg = mkMsg(now, '',
                        'You have been disconnected from the channel. ' +
                        'Please leave the channel and log on again.', 'meta');
        var msg2 = mkMsg(now, '', 'Error message: ' + reason, 'meta');
        addMessageToBuffer(dataObj, msg);
        addMessageToBuffer(dataObj, msg2);
        $('.chatinput', dataObj['chatDiv']).prop('disabled', true).
            addClass('disabled');
        $('.write_message', dataObj['chatDiv']).prop(
            'disabled', true).removeClass().addClass('cupid-disabled');
        $(dataObj['chatDiv']).focus();
    }

    var sendMessage = function(dataObj) {
        var chatDiv = dataObj['chatDiv'];
        var messageText = $('.chatinput', chatDiv).val();
        messageText = messageText.replace(/^\s+/, '').replace(/\s+$/, '');
        if (messageText == '') return;

        $('.chatinput', chatDiv).val('').focus();

        var now = (new Date()).valueOf() / 1000;
        var msg;
        if (messageText.match(/^\/me /)) {
            messageText = messageText.replace(/^\/me /, '');
            msg = { 'type': 'action',
                    'text': messageText };
        } else {
            msg = { 'type': 'talk',
                    'text': messageText };
        }

        scrollToBottom(dataObj);

        ajaxCall('/api/write',
                 dataObj,
                 msg,
                 function(data) {},
                 function(data, msg) {
                     fetchError(dataObj, data, reason);
                     cleanupOutstandingRequests(dataObj);
                 });
    }

    var ajaxCall = function(url, dataObj, json, success, failure,
                            skipApiWrap) {
        var sess = '';
        if (dataObj.session != '') sess = dataObj.session;
        var req;
        if (skipApiWrap) {
            req = JSON.stringify(json);
        } else {
            req = JSON.stringify({ session: sess,
                                   requestData: json });
        }

        var wrapSuccess = function(data, textStatus, jqXHR) {
            if (data.status != 'ok') {
                failure(data, data.reason);
            } else {
                dataObj['session'] = data.session;
                success(data.response);
            }
        };

        var wrapFailure = function(jqXHR, textStatus, errorThrown) {
            var obj = { status: 'failure',
                        statusCode: 'textStatus',
                        reason: errorThrown };
            failure(obj, obj.reason);
        }

        return $.ajax({
            url: url,
            data: req,
            success: wrapSuccess,
            error: wrapFailure,
            type: 'POST',
            cache: false,
            contentType: 'application/json',
            dataType: 'json',
            processData: false,
            timeout: 120 * 1000
        });
    }

    var handleLogin = function(dataObj) {
        var loginDiv = dataObj['loginDiv'];
        var userName = $('.username', loginDiv).val();
        userName = userName.replace(/^\s+/, '').replace(/\s+$/, '');
        dataObj['desiredUserName'] = userName;
        var $errorDiv = $('.error', loginDiv);
        var $helloDiv = $('.hello', loginDiv);

        var showError = function (msg) {
            $helloDiv.hide();
            $errorDiv.text(msg);
            $errorDiv.show('fade', 250).delay(
                5000).hide('fade', function() {
                    $helloDiv.show();
                });
        }

        if (userName == '') {
            showError("Error: username must not be empty.");
            return;
        }

        ajaxCall('/api/join',
                 dataObj,
                 { desiredUserName: userName },
                 function(data) { setupChatRoom(dataObj); },
                 function(data, msg) { showError(msg); },
                true);
    }

    var setupChatRoom = function(dataObj) {
        var loginDiv = dataObj['loginDiv'];
        var chatDiv = dataObj['chatDiv'];
        $(loginDiv).hide('fade', {}, 300, function() {
            $(chatDiv).show('fade', {}, 300);
        });
        var $buf = $('.chattext', chatDiv);
        var $button = $('.write_message', chatDiv);
        var $input = $('.chatinput', chatDiv);

        $('.top-message',chatDiv).text(
            dataObj['desiredUserName'] + '@snap-chat');

        $button.click(function () {
            sendMessage(dataObj);
        });

        $input.keypress(function(e) {
            if (e.which == 13) {
                sendMessage(dataObj);
                e.preventDefault();
            }
        });

        // $input.focusin(function() {
        //     scrollToBottom(dataObj);
        // });

        $input.bind('touchmove',function(e){
            e.preventDefault();
        });

        setTimeout(function() { $input.focus(); }, 500);
        fetchMessages(dataObj);
        return this;
    }

    var fetchMessages = function(dataObj) {
        dataObj['fetcher'] = ajaxCall(
            "/api/fetch", dataObj, {},
            function(data) { gotMessage(dataObj, data); },
            function(data, reason) { fetchError(dataObj, data, reason); }
        );
    }

    var handleLogout = function(dataObj) {
        cleanupOutstandingRequests(dataObj);
        var loginDiv = dataObj['loginDiv'];
        var chatDiv = dataObj['chatDiv'];
        ajaxCall(
            '/api/leave',
            dataObj,
            {},
            function(data) {},
            function(data) {});

        $(chatDiv).hide('fade', {}, 300, function() {
            initialize(dataObj.target);
        });
    }

    var initialize = function(obj) {
        obj.html('');
        var _loginDiv = $(loginHTML);
        obj.hide();
        obj.append(_loginDiv);
        var _chatDiv = $(chatRoomHTML);
        _chatDiv.hide();
        obj.append(_chatDiv);
        obj.show("fade", {}, 200);

        var dataObj = { loginDiv: _loginDiv,
                        chatDiv: _chatDiv,
                        target: obj,
                        session: '',
                        numMessages: 0
                      };

        setTimeout(function() {
            var $u = $('.username', _loginDiv);
            $u.focus().effect("highlight", 2000);
        }, 500);

        $('.username', _loginDiv).keypress(function(e) {
            if (e.which == 13) {
                handleLogin(dataObj);
                e.preventDefault();
            }
        });

        $(':button', _loginDiv).click(function() { handleLogin(dataObj) });
        $('.logoutButton', _chatDiv).click(function() { handleLogout(dataObj) });

        return(dataObj);
    }

    $.fn.snapChat = function() {
        var dataObj = initialize(this);
        var $this = $(this);
        this.data($this, 'snap_chat', dataObj);
        return this;
    };

})( jQuery );
