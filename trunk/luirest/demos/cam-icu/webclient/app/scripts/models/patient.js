/*global define*/

define([
    'underscore',
    'backbone'
], function (_, Backbone) {
    'use strict';

    var PatientModel = Backbone.Model.extend({
        defaults: {
        }
    });

    return PatientModel;
});
