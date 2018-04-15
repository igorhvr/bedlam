var express = require('express');
var app = express();
var vm = require('vm');

// tip from http://stackoverflow.com/a/17644882/450148
app.use(function(req, res, next){
   var data = "";
   req.on('data', function(chunk){ data += chunk});
   req.on('end', function() {
      req.rawBody = data;
      next();
   });
});

app.post('/', function(req, res) {
    console.log('Received: ' + req.rawBody);

    try {
	var result = vm.runInThisContext(req.rawBody);
	console.log('Result: ' + result);

	res.writeHead(200, {'Content-Type': 'text/plain'});
	res.end(JSON.stringify(result));
    } catch (err) {
	console.log('Error: ' + err);

	res.writeHead(400, {'Content-Type': 'text/plain'});
	res.end('false');
    }
});

app.listen(27429, '0.0.0.0');

console.log('Server running at http://js:27429/');
