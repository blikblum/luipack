/*global define*/

define([
    'jquery',
    'underscore',
    'backbone',
    'templates'
], function ($, _, Backbone, JST) {
    'use strict';

    var PatienteditView = Backbone.View.extend({
        template: JST['app/scripts/templates/patientedit.hbs']
    });

    return PatienteditView;
});
