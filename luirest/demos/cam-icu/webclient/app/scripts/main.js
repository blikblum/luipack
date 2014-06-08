/*global require*/
'use strict';

require.config({
    shim: {
        underscore: {
            exports: '_'
        },
        backbone: {
            deps: [
                'underscore',
                'jquery'
            ],
            exports: 'Backbone'
        },
        bootstrap: {
            deps: ['jquery'],
            exports: 'jquery'
        },
        handlebars: {
            exports: 'Handlebars'
        },
      stickit:{
        deps: ['backbone']
      }
    },
    paths: {
        jquery: '../bower_components/jquery/jquery',
        backbone: '../bower_components/backbone/backbone',
        underscore: '../bower_components/underscore/underscore',
        bootstrap: '../bower_components/sass-bootstrap/dist/js/bootstrap',
        handlebars: '../bower_components/handlebars/handlebars',
        stickit: '../bower_components/backbone.stickit/backbone.stickit',
        stickitform: '../bower_components/backbone.stickit.form/src/backbone.stickit.form',
        text: '../bower_components/requirejs-text/text'
    }
});

var app = app || {};
app.data = {}
app.BASE_URL = '../../luirest/camicu.cgi';
app.mainView = null;
app.setMainView = function(newView){
  if (app.mainView) {
    app.mainView.remove()
  }
  app.mainView = newView;
  $('#main').append(newView.$el);
}

require([
    'jquery',
    'backbone',
  'collections/patients',
  'routes/main',
  'stickit',
  'bootstrap'
], function ($, Backbone, PatientCollection, MainRouter) {

   $(document).ready(function(){
     if (app.started) return;
     console.log('app start');
     app.started = true;
     app.data.patients = new PatientCollection();
     app.mainRouter = new MainRouter();
     Backbone.history.start();
     app.data.patients.fetch({
       reset: true,
       success: function(){
         app.mainRouter.navigate('#patients', {trigger:true})
       }
     });
   })

});
