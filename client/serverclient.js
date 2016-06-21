// Require HTTP module (to start server) and Socket.IO
var express = require('express');
var http = require('http');
var net = require('net');
var port = 8080;
var app = express();
var messages = new Object();
var clients = new Object();
var id = 1;
app.engine('html', require('ejs').renderFile);


function connectToClient(username) {
var client = new net.Socket();
client.connect(5455, '127.0.0.1', function() {
  console.log('connected');
});
client.on('data', function(data) {
  console.log('Received: ' + data + ' for ' + username);
  if (messages[username] === undefined) {
    messages[username] = data.toString();
  } else {
    messages[username] = messages[username] + '&' + data.toString();
  }
//client.destroy();
});

client.on('close', function() {
	console.log('Connection closed');
});
return client;
}
// Add a connect listener

app.get('/', function(req, res) {
    res.render('client.html');
});

app.get('/startpage.html', function(req, res) {
    res.render('startpage.html');
});

app.get('/chatpage.html', function(req, res) {
    res.render('chatpage.html');
});

app.get('/userrequestmodaltemplate.html', function(req, res) {
  res.render('userrequestmodaltemplate.html');
});

app.get('/messagetemplate.html', function(req, res) {
  res.render('messagetemplate.html');
});

app.get('/messagetemplateown.html', function(req, res) {
  res.render('messagetemplateown.html');
});

app.get('/usertemplate.html', function(req, res) {
  res.render('usertemplate.html');
});
app.get('/get_updates', function(req, res) {
  var username = req.query.username;
  var msg = messages[username];
  messages[username] = undefined;
  res.send(msg);
});

app.get('/signout', function(req, res){
  var username = req.query.user;
  var client = clients[username];
  client.destroy();
  clients[username] = undefined;
});

app.get('/ajax', function(req, res) {
    id++;
    var command = req.query.command;
    var params = req.query.params;
    username = params.split(',')[0];
    if (clients[username] === undefined) {
      clients[username] = connectToClient(username);
    }
    var request = id;
    if (command == 'register' || command == 'auth') {
      request = request + ',' + command + ',' + params;
    } else {
      sp = params.split(',');
      request = request + ',' + command + ',' + sp[1];
      if (command == 'chat') {
        request = request + ',' + sp[2];
      }
    }
    console.log('Seding request ' + request);
    clients[username].write(request);
    res.end();
});

app.listen('8080', '127.0.0.1');

console.log('Server running at http://127.0.0.1:' + port + '/');

//x,register,nume,parola
//x,auth,nume,parola
//x,add_friend,nume
//x,accept_friend_request,nume
//x,reject_friend_request,nume
//x,chat,nume,mesaj
//x,group,mesaj



//x,friendship_status_sent
