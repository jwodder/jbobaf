// Appending new sets of form elements using .innerHTML caused the values of
// pre-existing elements to be reset, and it turns out that .innerHTML isn't
// even standard!  Hence, I have to add elements the long, OO way.  I feel like
// a caveman ... or a Java programmer.

var protoform;

function mkOption(selected, value, label) {
 var opt = document.createElement('option');
 if (selected) opt.setAttribute('selected', 'selected');
 opt.setAttribute('value', value);
 opt.textContent = label;
 return opt;
}

function initFormAddition() {
 protoform = document.createElement('p');
 protoform.className = 'link';
 var e1 = document.createElement('select');
 e1.name = 'bool';
 e1.appendChild(mkOption(true, 'and', 'and'));
 e1.appendChild(mkOption(false, 'or', 'or'));
 protoform.appendChild(e1);
 var e2 = document.createElement('select');
 e2.name = 'field';
 e2.appendChild(mkOption(true, 'valsi', 'valsi'));
 e2.appendChild(mkOption(false, 'rafsi', 'at least one rafsi'));
 e2.appendChild(mkOption(false, 'glico', 'keyword or definition'));
 e2.appendChild(mkOption(false, 'ralvla', 'keyword'));
 e2.appendChild(mkOption(false, 'selvla', 'definition'));
 protoform.appendChild(e2);
 var e3 = document.createElement('select');
 e3.name = 'relation';
 e3.appendChild(mkOption(true, 'is', 'is'));
 e3.appendChild(mkOption(false, 'contains', 'contains'));
 e3.appendChild(mkOption(false, 'regex', 'matches regex'));
 protoform.appendChild(e3);
 var e4 = document.createElement('input');
 e4.name = 'query';
 e4.setAttribute('type', 'text');
 e4.setAttribute('size', '20');
 e4.setAttribute('maxlength', '128');
 protoform.appendChild(e4);
 document.getElementById('suhi').disabled = false;
}

function addSubquery() {
 if (protoform) {
  document.getElementById('subqueries').appendChild(protoform.cloneNode(true));
  document.getElementById('vuhu').disabled = false;
 }
}

function removeSubquery() {
 var qty = document.getElementById('subqueries').childNodes.length;
 if (qty > 1) {
  document.getElementById('subqueries').removeChild(
   document.getElementById('subqueries').lastChild);
  if (qty == 2) document.getElementById('vuhu').disabled = true;
 }
}

function toggleCmavo() {
 document.getElementById('lujmaho').disabled =
  !document.getElementById('cmavo').checked;
}
