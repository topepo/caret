HTMLWidgets.widget({

  name: 'd3heatmap',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      lastTheme: null,
      lastValue: null
    };

  },
  
  renderValue: function(el, x, instance) {
    this.doRenderValue(el, x, instance);
  },

  // Need dedicated helper function that can be called by both renderValue
  // and resize. resize can't call this.renderValue because that will be
  // routed to the Shiny wrapper method from htmlwidgets, which expects the
  // wrapper data object, not x.
  doRenderValue: function(el, x, instance) {
    var self = this;
    
    instance.lastValue = x;
    
    if (instance.lastTheme && instance.lastTheme != x.theme) {
      d3.select(document.body).classed("theme-" + instance.lastTheme, false);
    }
    if (x.theme) {
      d3.select(document.body).classed("theme-" + x.theme, true);
    }

    el.innerHTML = "";
    
    this.loadImage(x.image, function(imgData, w, h) {
      
      if (w !== x.matrix.dim[0] || h !== x.matrix.dim[1]) {
        throw new Error("Color dimensions didn't match data dimensions")
      }
      
      var merged = [];
      for (var i = 0; i < x.matrix.data.length; i++) {
        var r = imgData[i*4];
        var g = imgData[i*4+1];
        var b = imgData[i*4+2];
        var a = imgData[i*4+3];
        merged.push({
          label: x.matrix.data[i],
          color: "rgba(" + [r,g,b,a/255].join(",") + ")"
        })
      }
      x.matrix.merged = merged;
      //console.log(JSON.stringify({merged: x.matrix.merged}, null, "  "));

      var hm = heatmap(el, x, x.options);
      if (window.Shiny) {
        var id = self.getId(el);
        hm.on('hover', function(e) {
          Shiny.onInputChange(id + '_hover', !e.data ? e.data : {
            label: e.data.label,
            row: x.matrix.rows[e.data.row],
            col: x.matrix.cols[e.data.col]
          });
        });
        /* heatmap doesn't currently send click, since it means zoom-out
        hm.on('click', function(e) {
          Shiny.onInputChange(id + '_click', !e.data ? e.data : {
            label: e.data.label,
            row: e.data.row + 1,
            col: e.data.col + 1
          });
        });
        */
  	  }
    });
  },

  resize: function(el, width, height, instance) {
    if (instance.lastValue) {
      this.doRenderValue(el, instance.lastValue, instance);
    }
  },
  
  loadImage: function(uri, callback) {
    var img = new Image();
    img.onload = function() {
      // Save size
      w = img.width;
      h = img.height;

      // Create a dummy canvas to extract the image data
      var imgDataCanvas = document.createElement("canvas");
      imgDataCanvas.width = w;
      imgDataCanvas.height = h;
      imgDataCanvas.style.display = "none";
      document.body.appendChild(imgDataCanvas);

      var imgDataCtx = imgDataCanvas.getContext("2d");
      imgDataCtx.drawImage(img, 0, 0);

      // Save the image data.
      imgData = imgDataCtx.getImageData(0, 0, w, h).data;

      // Done with the canvas, remove it from the page so it can be gc'd.
      document.body.removeChild(imgDataCanvas);
      
      callback(imgData, w, h);
    };
    img.src = uri;
  }

});
