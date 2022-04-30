<template>
  <div>
    <slot name="pre"></slot>
    <span class="distate distate-switcher">
      <input
      type="radio"
      :id="generateId(elem.value)"
      :value="elem.value"
      v-model="selected"
      v-for="(elem, i) in options"
      v-on:change="$emit('change', $event.target.value)"
      >
      <i></i>
      <label :for="generateId(elem.value)" v-text="elem.text" v-for="(elem, i) in options" v-if="!noLabel"></label>
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
        select: {
          required: true,
          default: '0'
        },
        noLabel: {
          type: Boolean,
          required: false,
          default: false
        },
        options: {
          type: Array,
          required: false,
          default: () => [
            {value: '0', text: 'Выключено'},
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
      watch: {
        'select': function (value) {
          this.selected = value;
        },
      },      
      mounted(){
        this.selected = this.select;
      },
      methods:{
        generateId(id){
          return 'switcher-item-state-' + id;
        },
      }
    }
</script>

<style scoped>
label{
  font-weight: 400 !important;
}
.distate {
  position: relative;
  display: inline-block;
}
.distate > input[type="radio"] {
  position: absolute;
  left: 0;
  top: 0;
  width: 18px;
  height: 18px;
  margin: 0;
  padding: 0;
  opacity: 0;
}
.distate > input[type="radio"] + i {
  position: relative;
  display: inline-block;
  width: 18px;
  height: 18px;
  vertical-align: top;
}
.distate > input[type="radio"] + i, .distate > input[type="radio"] + i:before, .distate > input[type="radio"] + i:after {
  transition: all 0.3s;
}
.distate > input[type="radio"]:first-child {
  z-index: 10;
}
.distate > input[type="radio"]:first-child:checked {
  z-index: 0;
}
.distate > input[type="radio"]:checked + input[type="radio"] {
  z-index: 10;
}
.distate > input[type="radio"]:checked + i + label {
  display: none;
}
.distate > input[type="radio"]:checked + input[type="radio"] + i + label + label {
  display: none;
}
.distate-switcher > input[type="radio"] {
  width: 46px;
  height: 24px;
  left: -3px;
  top: -1px;
}
.distate-switcher > input[type="radio"] + i {
  width: 40px;
  height: 16px;
  margin-top: 2px;
  background-color: #bdbdbd;
  border-radius: 8px;
}
.distate-switcher > input[type="radio"] + i:before {
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
.distate-switcher > input[type="radio"]:checked + i {
  background-color: #a5d7a7;
}
.distate-switcher > input[type="radio"]:checked + i:before {
  left: 19px;
  background-color: #4caf50;
}
.distate-switcher > input[type="radio"]:checked + input[type="radio"] + i {
  background-color: #f9a19a;
}
.distate-switcher > input[type="radio"]:checked + input[type="radio"] + i:before {
  background-color: #f44336;
}
</style>