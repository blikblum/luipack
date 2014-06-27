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
//app.BASE_URL = '../cgi-bin/camicu.cgi';
app.mainView = null;
app.setMainView = function(newView){
  if (app.mainView) {
    app.mainView.remove()
  }
  app.mainView = newView;
  $('#main').append(newView.$el);
}

var toOADate = (function () {
    var epoch = new Date(1899,11,30);
    var msPerDay = 8.64e7;

    return function(d) {
        var v = -1 * (epoch - d)/msPerDay;

        // Deal with dates prior to 1899-12-30 00:00:00
        var dec = v - Math.floor(v);

        if (v < 0 && dec) {
            v = Math.floor(v) - dec;
        }

        return v;
    }
}());


var fromOADate = (function() {
    var epoch = new Date(1899,11,30);
    var msPerDay = 8.64e7;

    return function(n) {
        // Deal with -ve values
        var dec = n - Math.floor(n);

        if (n < 0 && dec) {
            n = Math.floor(n) - dec;
        }

        return new Date(n*msPerDay + +epoch);
    }
}());

require([
    'jquery',
    'backbone',
  'collections/patients',
  'routes/main',
  'handlebars',
  'stickit',
  'bootstrap',
], function ($, Backbone, PatientCollection, MainRouter, Handlebars) {

   $(document).ready(function(){
     if (app.started) return;
     console.log('app start');
     Handlebars.registerHelper('dateToStr', function(val){
         return fromOADate(val).toLocaleDateString();
     });
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
