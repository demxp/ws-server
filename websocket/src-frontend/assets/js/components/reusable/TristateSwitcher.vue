<template>
  <div v-if="!!mode">
    <slot name="pre"></slot>
      <span :class="swClass">
        <input
        type="radio"
        :id="generateId(elem.value)"
        :value="elem.value"
        v-model="selected"
        v-for="(elem, i) in options"
        v-on:change="$emit('change', $event.target.value)"
        >
        <i></i>
        <label :for="generateId(elem.value)" v-text="elem.text" v-for="(elem, i) in options"></label>
      </span>
    <slot name="post"></slot>
  </div>
</template>

<script>
    export default {
      model: {
        prop: 'select',
        event: 'change'
      },
      props: {
        mode: {
          type: String,
          required: true,
          default: 'checkbox',
          validator: function (value) {
            return ['checkbox', 'switcher', 'rotate'].includes(value)
          }
        },
        select: {
          required: true,
          default: '0'
        },
        options: {
          type: Array,
          required: false,
          default: () => [
            {value: '-1', text: 'Выключено'},
            {value: '0', text: 'Не задано'},
            {value: '1', text: 'Включено'}
          ],
          validator: function (value) {
            let still = true;
              if(typeof(k) == 'Object'){
                for(let i in value){
                  if(!('value' in value[i]) || !('text' in value[i])) still = false;
                }
              }
            if(still) return true;
            console.error('Элементы массива "options" должны содержать value и text');
            return false;
          }          
        }
      },
      data(){
        return{
          selected: null
        }
      },
      mounted(){
        this.selected = this.select;
      },
      watch: {
        'select': function (value) {
          this.selected = value;
        },
      },      
      methods:{
        generateId(id){
          return 'switcher-item-state-' + id;
        },
      },
      computed:{
        swClass: function () {
          return 'tristate tristate-' + this.mode;
        }
      }
    }
</script>

<style scoped>
label{
  font-weight: 400 !important;
}
.tristate {
  position: relative;
  display: inline-block;
}
.tristate > input[type="radio"] {
  position: absolute;
  left: 0;
  top: 0;
  width: 18px;
  height: 18px;
  margin: 0;
  padding: 0;
  opacity: 0;
}
.tristate > input[type="radio"] + i {
  position: relative;
  display: inline-block;
  width: 18px;
  height: 18px;
  vertical-align: top;
}
.tristate > input[type="radio"] + i, .tristate > input[type="radio"] + i:before, .tristate > input[type="radio"] + i:after {
  transition: all 0.3s;
}
.tristate > input[type="radio"]:first-child {
  z-index: 10;
}
.tristate > input[type="radio"]:first-child:checked {
  z-index: 0;
}
.tristate > input[type="radio"]:checked + input[type="radio"] {
  z-index: 10;
}
.tristate > input[type="radio"]:checked + i + label, .tristate > input[type="radio"]:checked + i + label + label {
  display: none;
}
.tristate > input[type="radio"]:checked + input[type="radio"] + i + label, .tristate > input[type="radio"]:checked + input[type="radio"] + i + label + label + label {
  display: none;
}
.tristate > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i + label + label, .tristate > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i + label + label + label {
  display: none;
}
.tristate-checkbox > input[type="radio"] + i {
  border: solid 2px #bdbdbd;
  border-radius: 3px;
}
.tristate-checkbox > input[type="radio"] + i:before {
  content: ' ';
  display: block;
  position: absolute;
  left: -2px;
  top: -2px;
  right: -2px;
  bottom: -2px;
  background-color: #8b8b8b;
  border-radius: 3px;
}
.tristate-checkbox > input[type="radio"] + i:after {
  content: ' ';
  display: block;
  position: absolute;
  left: 4px;
  top: 7px;
  color: white;
  border-bottom: solid 3px;
  opacity: 0;
}
.tristate-checkbox > input[type="radio"]:checked + i {
  background-color: #009688;
}
.tristate-checkbox > input[type="radio"]:checked + i:before {
  background-color: #009688;
}
.tristate-checkbox > input[type="radio"]:checked + i:after {
  left: 1px;
  top: 3px;
  width: 14px;
  height: 5px;
  border-left: solid 3px;
  border-bottom: solid 3px;
  transform: rotate(-45deg);
  opacity: 1;
}
.tristate-checkbox > input[type="radio"]:checked + input[type="radio"] + i:after {
  width: 10px;
  height: 0px;
  border-left: none;
  border-bottom: solid 3px;
  transform: rotate(0);
  opacity: 1;
}
.tristate-checkbox > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i:before {
  opacity: 0;
  transform: scale(0);
}
.tristate-switcher > input[type="radio"] {
  width: 46px;
  height: 24px;
  left: -3px;
  top: -1px;
}
.tristate-switcher > input[type="radio"] + i {
  width: 40px;
  height: 16px;
  margin-top: 2px;
  background-color: #bdbdbd;
  border-radius: 8px;
}
.tristate-switcher > input[type="radio"] + i:before {
  content: ' ';
  position: absolute;
  top: -3px;
  left: -3px;
  display: block;
  width: 24px;
  height: 24px;
  background-color: #fafafa;
  border-radius: 50%;
  box-shadow: 0 1px 8px 1px rgba(0, 0, 0, 0.3);
}
.tristate-switcher > input[type="radio"]:checked + i {
  background-color: #a5d7a7;
}
.tristate-switcher > input[type="radio"]:checked + i:before {
  left: 19px;
  background-color: #4caf50;
}
.tristate-switcher > input[type="radio"]:checked + input[type="radio"] + i:before {
  left: 8px;
}
.tristate-switcher > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i {
  background-color: #f9a19a;
}
.tristate-switcher > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i:before {
  background-color: #f44336;
}
.tristate-rotate label {
  display: inline-block;
  padding-top: 3px;
  padding-left: 3px;
}
.tristate-rotate > input[type="radio"] {
  width: 30px;
  height: 30px;
}
.tristate-rotate > input[type="radio"] + i {
  width: 24px;
  height: 24px;
  margin: 5px;
  background-color: #ccc;
  border-radius: 50%;
  box-shadow: 3px 3px 5px rgba(0, 0, 0, 0.3), inset 1px 1px 2px rgba(255, 255, 255, 0.4), inset -1px -1px 2px rgba(0, 0, 0, 0.4);
}
.tristate-rotate > input[type="radio"] + i:before {
  content: ' ';
  display: block;
  position: absolute;
  left: 50%;
  top: 50%;
  width: 3px;
  height: 3px;
  margin-left: -1px;
  margin-top: -1px;
  border-radius: 50%;
  box-shadow: -16px 0 0 #8b8b8b, 0 -16px 0 #8b8b8b, 16px 0 0 #8b8b8b;
}
.tristate-rotate > input[type="radio"] + i:after {
  content: ' ';
  display: block;
  position: absolute;
  left: 0;
  top: 50%;
  width: 10px;
  margin-top: -1px;
  border-bottom: solid 2px #666;
  transform-origin: 12px 50%;
}
.tristate-rotate > input[type="radio"]:checked + i:after {
  transform: rotate(180deg);
}
.tristate-rotate > input[type="radio"]:checked + input[type="radio"] + i:after {
  transform: rotate(90deg);
}
.tristate-rotate > input[type="radio"]:checked + input[type="radio"] + input[type="radio"] + i:after {
  transform: rotate(0deg);
}
</style>