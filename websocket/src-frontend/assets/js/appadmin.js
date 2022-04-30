window.Vue = require('vue');

import swal from 'sweetalert'
import $ from 'jquery';
import { WindowInstallCustom } from './components/window.functions.module.js';
 
global.jQuery = $;
global.$ = $;

WindowInstallCustom();

/**
 * Next, we will create a fresh Vue application instance and attach it to
 * the page. Then, you may begin adding components to this application
 * or customize the JavaScript scaffolding to fit your unique needs.
 */


function Ws(url){
  this.ws = undefined;
  this.cb = [];
  this.sendBuffer = [];
  this.url = url;
  this.wsopenstate = 'CLOSED';
  this.onopenfun = false;
  this.onclosefun = false;

  this.addCb = function (cbkey, cbfun, delAfter){
    this.cb.push({
      key: cbkey,
      fun: cbfun,
      deleteAfter: delAfter
    });      
  }.bind(this);
  this.isOpened = function (){
    return this.wsopenstate;
  }.bind(this);
  this.getCb = function (){
    console.log(this.cb);
  }.bind(this);
  this.send = function (data, cbfun=null, delAfter=true, cbkey=false){
    if(!!cbfun){
      if(!cbkey) cbkey = data.action;
      this.addCb(cbkey, cbfun, delAfter);
    }
    if(this.isOpened() == 'OPENED'){
      return this.ws.send(JSON.stringify(data));      
    }
    if(this.isOpened() == 'CLOSED'){
      this.startWs(this.url);
    }
    return this.sendBuffer.push(data);
  }.bind(this);
  this.startWs = function (url){
    this.wsopenstate = 'OPENING';
    return new Promise(function(resolve, reject) {
      this.ws = new WebSocket("ws://" + url);
      this.ws.onopen = function() {
        this.wsopenstate = 'OPENED';
        console.log("WEBSOCKET OPENED");
        if(typeof(this.onopenfun) == 'function') this.onopenfun();
        this.pinger = setInterval(function() {
          this.send({action: 'ping'});
        }.bind(this), 20000);
        resolve({
          ws: this.ws,
          sbuf: this.sendBuffer
        });
      }.bind(this);
      this.ws.onclose = function(e) {
        this.wsopenstate = 'CLOSED';
        console.log("WEBSOCKET CLOSED");
        if(typeof(this.onclosefun) == 'function') this.onclosefun();
        clearInterval(this.pinger);
        this.pinger = 0;
      }.bind(this);
      this.ws.onmessage = function(e) {
        let data = JSON.parse(e.data);
        this.cb = this.cb.reduce(function(acc, item, index, array) {
          let execute = null;
          if(item.key == data.action){
            execute = item;
            if(!item.deleteAfter){
              acc.push(item);
            }
            if(!!execute) setTimeout(execute.fun(data), 50);
          }else{
            acc.push(item);
          }
          return acc;
        }, []);          
      }.bind(this);
    }.bind(this)).then((ac) => {
      for (let i = 0; i < ac.sbuf.length; i++) {
        ac.ws.send(JSON.stringify(ac.sbuf[i]));
      }
    })
  }.bind(this);

  return {
    send: this.send,
    isOpened: this.isOpened,
    addCallback: this.addCb,
    getCallbacks: this.getCb
  };
};

Vue.component('UsersIndex', require('./components/admin/UsersIndex.vue'));
Vue.component('UsersEdit', require('./components/admin/UsersEdit.vue'));

import MainBlock from './components/admin/MainBlock.vue';

const app = new Vue({
  el: '#vueapp',
  components: {MainBlock},
  data(){
    return{
      isAuth: false
    }
  },
  mounted(){
    this.refreshTokens(this.wsinit);
  },
  methods:{
    refreshTokens(callback=null){
      let xhr = new XMLHttpRequest();
      xhr.open('GET', '/auth/refresh');
      xhr.withCredentials = true;
      let _this = this;
      new Promise(function(resolve, reject) {
        xhr.onload = function(){
          if(xhr.status != 200 && xhr.status != 201){
            return reject({name: 'StatusError', message: xhr.response});
          }else{
            return resolve(xhr.response);
          }
        };
        xhr.send();
      }).catch((e) => {
        if(e.name == 'StatusError'){
          window.location.replace("/auth/login");
        }
      }).then(data => {
        let json = JSON.parse(data);
        _this.token = json.access.token;
        _this.isAuth = true;
        if(!!callback) callback();
      })
    },
    wsinit(){
      this.ws = new Ws(window.location.host + "/websocket");
    },
    wssend(data, cbfun=null, delAfter=true, cbkey=false){
      if(!!this.token) data.token = this.token;
      return this.ws.send(data, cbfun, delAfter, cbkey);
    },
    parseTokenData(){
      let df = b64DecodeUnicode(this.token.split('.')[1]);
      this.user = JSON.parse(df);
      console.log(this.user);
    }
  }
});