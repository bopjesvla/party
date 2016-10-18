// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
import "phoenix_html"

import Vue from 'vue'
import VueRouter from 'vue-router'
import Home from './Home.vue'
import Lobby from './Lobby.vue'
import LobbySidebar from './LobbySidebar.vue'

import 'vueify/lib/insert-css'

Vue.use(VueRouter)

let router = new VueRouter({
  routes: [{
    path: '/',
    component: Home,
    children: [
      {
        path: '/',
        name: 'lobby',
        components: {
          main: 'lobby',
          sidebar: LobbySidebar,
          default: Lobby
        }
      }
    ]
  }],
  mode: 'history',
  base: '/home'
})

let app = new Vue({
  router,
  el: '#view',
  render: h => h('router-view'),
  components: {Home},
})

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

import socket from "./js/socket"
