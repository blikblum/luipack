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
      },
        maskedinput: {
          deps: ['jquery']
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
        text: '../bower_components/requirejs-text/text',
        validation: '../bower_components/backbone-validation/dist/backbone-validation-amd',
        maskedinput: '../bower_components/jquery.maskedinput/jquery.maskedinput',
        localstorage: '../bower_components/backbone.localstorage/backbone.localStorage'
    }
});

var app = app || {};
app.data = {}
app.BASE_URL = '../../luirest/camicu.cgi';
//app.BASE_URL = '../cgi-bin/camicu.cgi';
//app.useLocalStorage = true;
app.mainView = null;
app.setMainView = function(newView, currentRoute){
  if (app.mainView) {
    app.mainView.remove()
  }
  if (currentRoute){
      app.previousRoute = currentRoute;
      $('#back-button').parent().removeClass('invisible');
  } else {
      delete app.previousRoute;
      $('#back-button').parent().addClass('invisible');
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
  'validation',
  'stickit',
  'bootstrap',
  'maskedinput'
], function ($, Backbone, PatientCollection, MainRouter, Handlebars, Validation) {

    Validation.configure({
        forceUpdate: true
    });

    _.extend(Validation.messages, {
        required: 'Campo obrigatório'
    });

    _.extend(Validation.callbacks, {
        valid: function (view, attr, selector) {
            var $el = view.$('[name=' + attr + ']'),
                $group = $el.closest('.form-group');

            $group.removeClass('has-error');
            $group.find('.help-block').html('').addClass('hidden');
        },
        invalid: function (view, attr, error, selector) {
            var $el = view.$('[name=' + attr + ']'),
                $group = $el.closest('.form-group');

            $group.addClass('has-error');
            $group.find('.help-block').html(error).removeClass('hidden');
        }
    });

   $(document).ready(function(){
     if (app.started) return;
     console.log('app start');
       $('#back-button').on('click', function (e) {
           e.preventDefault();
           if (app.previousRoute) {
               app.mainRouter.navigate(app.previousRoute, true);
           }
       })
     Handlebars.registerHelper('dateToStr', function(val){
         if(val){
             return fromOADate(val).toLocaleDateString('pt-BR');
         }

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
