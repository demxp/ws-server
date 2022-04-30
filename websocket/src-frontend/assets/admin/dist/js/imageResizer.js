  function addpicture (btn, parameters) {
  	(function(document, param){
		param = (function(p){
			return {
				'noresize': !!p.noresize,
				'nosquare': !!p.nosquare,
				'size': (!!p.size && p.size > 0) ? p.size : 0,
				'sizew': (!!p.sizew && p.sizew > 0) ? p.sizew : 0,
				'img_element_id': (!!p.img_element_id) ? p.img_element_id : false,
				'field_element_id': (!!p.field_element_id) ? p.field_element_id : false
			};
		})(param)

		if((param.size == 0 && !param.noresize && !param.nosquare) || (!param.noresize && param.nosquare && param.sizew == 0) || (!param.img_element_id || !param.field_element_id)){
			alert("INCORRECT IMGRESIZER CALL PARAMS");
			console.log("INCORRECT IMGRESIZER CALL PARAMS", '\r', param);
			return false;
		}

		param.script_alert = (function(document, param){
			var image_blk = document.getElementById(param.img_element_id).parentNode;
			var getRectParent = image_blk.getBoundingClientRect();
			var getRectElem = document.getElementById(param.img_element_id).getBoundingClientRect();

			var alert_blk = document.createElement('div');
			alert_blk.style.position = 'absolute';
			alert_blk.style.width = (getRectElem.right - getRectElem.left) + 'px';
			alert_blk.style.height = (getRectElem.bottom - getRectElem.top) + 'px';
			alert_blk.style.lineHeight = (getRectElem.bottom - getRectElem.top) + 'px';
			alert_blk.style.zIndex = 999;
			alert_blk.style.backgroundColor = 'white';
			alert_blk.style.opacity = 0.9;
			alert_blk.style.textAlign = 'center';
			alert_blk.style.verticalAlign = 'middle';
			alert_blk.style.id = 'script_alert';
			alert_blk.style.top = (getRectElem.top - getRectParent.top) + 'px';
			alert_blk.style.left = (getRectElem.left - getRectParent.left) + 'px';

			var img_alert_blk = document.createElement('div');
			img_alert_blk.style.width = '70px';
			img_alert_blk.style.height = '70px';
			img_alert_blk.style.display = 'inline-block';
			img_alert_blk.innerHTML = '<img src="\/img\/loading.gif" \/>';

			alert_blk.appendChild(img_alert_blk);
			return {
				'showAlert': function(){
					return image_blk.appendChild(alert_blk);	
				},
				'hideAlert': function(){
					return image_blk.removeChild(alert_blk);	
				}
			};
		})(document, param);

  		function init(){
  			return new Promise(function(resolve, reject) {
		 		var fileselect = document.createElement('input');
				fileselect.type = 'file';
		 		fileselect.onchange = function(){
		 			var selected = this.files[0];
			  		if (!selected.type.match('image.*')) {
			        	return reject({
			        		text: "Можно загружать только изображения!",
			        		errorcode: "Upload no image file"
			        	});			  			
			  		}
			        return resolve(URL.createObjectURL(selected));
		  		};
		  		var event = new MouseEvent("click");
		  		fileselect.dispatchEvent(event);
  			})
  		};

		function imageLoader(src) {
			return new Promise(function(resolve, reject){
				var img = document.createElement("img");
				img.onload = function() {
					return resolve(img);
				};
				img.onerror = function(e) {
					return reject({
			        	text: "Ошибка загрузки",
			        	errorcode: e
			        });
				};
				img.src = src;				
			})
		}

		function cropToSquare(img){
			var left, top, side;
			if(img.height > img.width){
				left = 0;
				top = (img.height - img.width)/2;
				side = img.width;
			}else if(img.height < img.width){
				left = (img.width - img.height)/2;
				top = 0;		
				side = img.height;		
			}else{
				left = 0;
				top = 0;
				side = img.width;
			}

		    var canvas = document.createElement('canvas');
		    canvas.width = side;
		    canvas.height = side;
		    canvas.getContext('2d').drawImage(img, left, top, img.width, img.height, 0, 0, img.width, img.height);
		    img.src = 'about:blank';
		    img.width = 1;
		    img.height = 1;
		    img = canvas;		    
		    return {
		    	img: img,
		    	side: side
		    };
		}

		function convertToCanvas(img){
		    var canvas = document.createElement('canvas');
		    canvas.width = img.width;
		    canvas.height = img.height;
		    canvas.getContext('2d').drawImage(img, 0, 0);
		    img.src = 'about:blank';
		    img.width = 1;
		    img.height = 1;
		    img = canvas;		    
		    return img;
		}		


		function resize(img, w, h) {
			return new Promise(function(resolve, reject){
			  setTimeout(function(){
			    console.log(img.width, img.height);

			    var steps = Math.ceil(Math.log(img.width / w) / Math.LN2);
			    var sW = w * Math.pow(2, steps - 1);
			    var sH = ((!!h) ? h : w) * Math.pow(2, steps - 1);
			    var x = 2;

			    function run() {
			      if ( ! (steps--)) {
			        return resolve(img);
			      }

			      setTimeout(function() {
			        console.log(sW, sH);
			        var canvas = document.createElement('canvas');
			        canvas.width = sW;
			        canvas.height = sH;
			        canvas.getContext('2d').drawImage(img, 0, 0, sW, sH);
			        img.src = 'about:blank';
			        img.width = 1;
			        img.height = 1;
			        img = canvas;

			        sW = Math.round(sW / x);
			        sH = Math.round(sH / x);
			        run();
			      }, 0);
			    }
			    run();
			  }, 0);
			})
		}


  		init().then(function(src){
  			return imageLoader(src)
  		}).then(function(img){
  			return new Promise(function(resolve, reject){
  				param.script_alert.showAlert();
  				setTimeout(function() {
  					resolve(img);	
  				}, 0);
  			})
  		}).then(function(img){
  			URL.revokeObjectURL(img.src);  			
  			if(param.nosquare && param.noresize){
  				return convertToCanvas(img);
  			}
  			if(param.nosquare && param.sizew > 0){
  				if(param.sizew >= img.width){
  					return convertToCanvas(img);
  				}
  				var sizeh = Math.round(param.sizew * img.height / img.width);
      			return resize(img, param.sizew, sizeh);
  			}
  			if(param.noresize){
  				return (cropToSquare(img)).img;
  			}  			
  			var cropped = cropToSquare(img);
  			if(param.size >= cropped.img.width){
  				return cropped.img;
  			}  			
      		return resize(cropped.img, param.size);
  		}).then(function(img){
  			document.getElementById(param.img_element_id).src = img.toDataURL("image/png");
  			document.getElementById(param.field_element_id).value = img.toDataURL("image/png");
  			console.log("FINAL");
  			param.script_alert.hideAlert();
  		}).catch(function(error){
  			param.script_alert.hideAlert();
  			if(!!error.text){
  				alert(error.text);
  				console.error(error.errorcode);
  			}else{
  				alert(error.message);
  				console.log(error);  				
  			}
  		})
  	})(document, parameters);
};