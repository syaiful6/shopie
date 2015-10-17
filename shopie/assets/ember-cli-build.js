/* global require, module */
var EmberApp = require('ember-cli/lib/broccoli/ember-app');

module.exports = function(defaults) {
  var app = new EmberApp(defaults, {
    // Add options here
  });

  // Use `app.import` to add additional libraries to the generated
  // output files.
  //
  // If you need to use different assets in different
  // environments, specify an object as the first parameter. That
  // object's keys should be the environment name and the values
  // should be the asset to use in that environment.
  //
  // If the library that you are including contains AMD or ES6
  // modules that you would like to import into your application
  // please specify an object with the list of modules as keys
  // along with the exports of each module as its value.
  app.import('bower_components/babel-polyfill/browser-polyfill.js', { prepend: true });
  app.import('bower_components/validator-js/validator.js');
  app.import('bower_components/rangyinputs/rangyinputs-jquery-src.js');
  app.import('bower_components/commonmark/dist/commonmark.js');
  app.import('bower_components/moment/moment.js');
  app.import('bower_components/keymaster/keymaster.js');
  app.import('bower_components/devicejs/lib/device.js');
  app.import('bower_components/jquery-ui/jquery-ui.js');
  app.import('bower_components/jquery-file-upload/js/jquery.fileupload.js');
  app.import('bower_components/blueimp-load-image/js/load-image.all.min.js');
  app.import('bower_components/jquery-file-upload/js/jquery.fileupload-process.js');
  app.import('bower_components/jquery-file-upload/js/jquery.fileupload-image.js');
  app.import('bower_components/jqueryui-touch-punch/jquery.ui.touch-punch.js');
  app.import('bower_components/google-caja/html-css-sanitizer-bundle.js');
  app.import('bower_components/codemirror/lib/codemirror.js');
  app.import('bower_components/codemirror/mode/htmlmixed/htmlmixed.js');
  app.import('bower_components/codemirror/mode/xml/xml.js');
  app.import('bower_components/codemirror/mode/css/css.js');
  app.import('bower_components/codemirror/mode/javascript/javascript.js');
  app.import('bower_components/xregexp/xregexp-all.js');
  app.import('bower_components/password-generator/lib/password-generator.js');
  app.import('bower_components/blueimp-md5/js/md5.js');

  // 'dem Styles
  app.import('bower_components/codemirror/lib/codemirror.css');
  app.import('bower_components/codemirror/theme/xq-light.css');

  return app.toTree();
};