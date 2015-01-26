var socr = socr|| {};

socr.distribution = (function(win){

    var data = function(a, b, s){
        this.lowerValue = a;
        this.upperValue = b;
        this.step = s;
        var size = 0, value, sum = 0, sumSquares = 0, mode, minValue, maxValue;
        var n = Math.round((this.upperValue - this.lowerValue) / this.step) + 1;
        var freq = new Array(n);
        for(i = 0; i < n; i++) freq[i] = 0;

        this.setValue = function(x){
            size++;
            sum += x;
            sumSquares += x * x;
            if (x < minValue) minValue = x;
            if (x > minValue) maxValue = x;
            freq[this.index(x)]++;
        }

        this.index = function(x){
            return Math.round((x - this.lowerValue) / this.step);
        }

        this.mean = function(x){
            return x / size;
        }

        this.variance = function(x){
            return sumSquares / (size - 1) - sum * sum / (size * (size - 1));
        }

        this.stdDev = function(){
            return Math.sqrt(this.variance());
        }

        this.getFreq = function(x){
            return freq[this.index(x)];
        }

        this.getFreq = function(x){
            return freq[this.index(x)] / size;
        }

        this.density = function(x){
            return this.getFreq(x) / (size * this.step);
        }

        this.getSize = function(){
            return size;
        }

        this.reset = function(){
            sum = 0; sumSquares = 0; size = 0;
            // revisit -- minValue is getting set to upper and max to lower
            minValue = this.upperValue; maxValue = this.lowerValue;
            for(i = 0; i < n; i++) freq[i] = 0;
        }
    }

    // General probability distribution
    var distribution = function (){
        this.minValue = 1;
        this.maxValue = 6;
        this.step = 1;
        this.type = 0;
        this.data = new data(this.minValue, this.maxValue, this.step);

        this.maxDensity = function(){
            var d = 0;
            for (var x = this.minValue; x <= this.maxValue; x = x + this.step) if (this.density(x) > d) d = this.density(x);
            return d;
        }

        this.density = function(x){
            return 1 / 6;
        }

        this.CDF = function(y){
            var p = 0, dx;
            if (this.type == 0) dx = 1; else dx = this.step;
            for (var x = this.minValue; x <= y; x = x + this.step) p = p + this.density(x) * dx;
            return p;
        }

        this.quantile = function(p){
            var x, x1, x2, error, q, n, i;
            if (p <= 0) return this.minValue;
            else if (p >= 1) return this.maxValue;
            else{
                x1 = this.minValue; x2 = this.maxValue;
                x = (x1 + x2) / 2;
                q = this.CDF(x);
                error = Math.abs(q - p);
                n = 1;
                while (error > 0.0001 && n < 100){
                    n++;
                    if (q < p) x1 = x; else x2 = x;
                    x = (x1 + x2) / 2;
                    q = this.CDF(x);
                    error = Math.abs(q - p);
                }
                return x;
            }
        }

        this.mean = function(){
            var m = 0, dx;
            if (this.type == 0) dx = 1; else dx = this.step;
            for (var x = this.minValue; x <= this.maxValue; x = x + this.step) m = m + x * this.density(x) * dx;
            return m;
        }

        this.variance = function(){
            var m = this.mean(), m2 = 0, dx;
            if (this.type == 0) dx = 1; else dx = this.step;
            for (var x = this.minValue; x <= this.maxValue; x = x + this.step) m2 = m2 + x * x * this.density(x) * dx;
            return m2 - m * m;
        }

        this.stdDev = function(){
            return Math.sqrt(this.variance());
        }

        this.simulate = function(){
            var x = this.quantile(Math.random());
            this.setValue(x);
            return x;
        }       

        this.setValue = function(x){
            this.data.setValue(x);
        }   

    }

    //The Binomial Distribution
    var _binomial = function (trials, prob){
        //Properties
        this.prob = prob;
        this.trials = trials;
        this.type = 0;
        this.minValue = 0;
        this.maxValue = this.trials;
        this.step = 1;
        this.data = new Data(0, this.trials, this.step);

        //Methods
        this.density = function(x){
            var k = Math.round(x);
            return binomial(this.trials, k) * Math.pow(this.prob, k) * Math.pow(1 - this.prob, this.trials - k);
        }

        this.mode = function(){
            if (prob == 1) return this.trials;
            else return Math.floor((this.trials + 1) * prob);
        }

        this.maxDensity = function(){
            return this.density(this.mode());
        }

        this.mean = function(){
            return this.trials * this.prob;
        }

        this.variance = function(){
            return this.trials * this.prob * (1 - this.prob);
        }

        this.simulate = function(){
            var successes = 0;
            for (var i = 1; i <= this.trials; i++){
                if (Math.random() < this.prob) successes++;
            }
            this.setValue(successes);
            return successes;
        }
    }
    _binomial.prototype = new distribution;


    var _locationScale = function (dist, location, scale){
        //Properties
        this.dist = dist;
        this.location = location;
        this.scale = scale;
        this.minValue = this.location + this.scale * this.dist.minValue;
        this.maxValue = this.location + this.scale * this.dist.maxValue;
        this.step = this.scale * this.dist.step;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        this.type = this.dist.type;

        //Methods
        this.density = function(x){
            var y = this.dist.minValue + Math.round((x - this.minValue) / this.step) * this.dist.step;
            if (this.type == 0) return this.dist.density(y);
            else return this.dist.density(y) / this.scale;
        }

        this.mode = function(){
            return this.location + this.scale * this.dist.mode;
        }

        this.maxDensity = function(){
            if (this.type == 0) return this.dist.maxDensity();
            else return this.dist.maxDensity() / this.scale;
        }

        this.mean = function(){
            return this.location + this.scale * this.dist.mean();
        }

        this.variance = function(){
            return this.scale * this.scale * this.dist.variance();
        }

        this.simulate = function(){
            var x = this.location + this.scale * this.dist.simulate();
            this.setValue(x);
            return x;
        }   
    }
    _locationScale.prototype = new distribution;



    var _convolution = function (d, n){
        //Properties
        this.dist = d;
        this.power = n;
        this.minValue = this.power * this.dist.minValue;
        this.maxValue = this.power * this.dist.maxValue;
        this.step = this.dist.step;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        this.type = this.dist.type;

        //Compute and cache the convolution pdf
        var a = this.dist.minValue, b = this.dist.maxValue, s = this.dist.step;
        var m = Math.round((b - a) / s) + 1;
        var delta = 1;
        if (this.type == 1) delta = this.step;
        var pdf = new Array(this.power);
        for (var k = 0; k < n; k++) pdf[k] = new Array((k + 1) * m - k);
        for (var j = 0; j < m; j++) pdf[0][j] = this.dist.density(a + j * s);
        for (var k = 1; k < n; k++){
            for (var j = 0; j < (k + 1) * m - k; j++){
                var sum = 0;
                for (var i = Math.max(0, j - m + 1); i < Math.min(j + 1, k * m - k + 1); i++)   sum = sum + pdf[k - 1][i] * pdf[0][j - i] * delta;
                pdf[k][j] = sum;
            }
        }

        //Methods
        this.density = function(x){
            var index = Math.round((x - this.minValue) / this.step);
            return pdf[this.power - 1][index];
        }

        this.mean = function(){
            return this.power * this.dist.mean();
        }

        this.variance = function(){
            return this.power * this.dist.variance();
        }

        this.simulate = function(){
            var sum = 0;
            for (i = 1; i <= this.power; i++) sum = sum + this.dist.simulate();
            this.setValue(sum);
            return sum;
        }
    }
    _convolution.prototype = new distribution;

    
    //Distribution of an order statistic from a given distribution
    var _orderStatistic = function (dist, sample, order){
        //Properties
        this.dist = dist;
        this.sample = sample;
        this.order = order; 
        this.type = this.dist.type;
        this.minValue = this.dist.minValue;
        this.maxValue = this.dist.maxValue;
        this.step = this.dist.step;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        //Methods
        this.density =  function(x){
            if (this.type == 0) return this.CDF(x) - this.CDF(x - this.step);
            else {
                var p = this.CDF(x);
                return this.order * binomial(this.sample, this.order) * Math.pow(p, this.order - 1) * Math.pow(1 - p, this.sample - this.order) * this.dist.density(x);
            }
        }
            
        this.CDF = function(x){
            var sum = 0, p = this.dist.CDF(x);
            for (var j = this.order; j <= this.sample; j++) sum = sum + binomial(this.sample, j) * Math.pow(p, j) * Math.pow(1 - p, this.sample - j);
            return sum;
        }
        
                
        this.simulate = function(){
            sampleValues = new Array(this.sample);
            orderStats = new Array(this.sample);
            for (var i = 0; i < this.sample; i++) sampleValues[i] = this.dist.simulate();
            orderStats = sampleValues.sort(ascend);
            var x = orderStats[order - 1];
            this.setValue(x);
            return x;
        }
    }
    _orderStatistic.prototype = new distribution;

    //This binomial distribution with the number of trials randomized
    var _binomialN = function (dist, prob){
        //Properties
        this.dist = dist;
        this.prob = prob;
        this.minValue = 0;
        this.maxValue = this.dist.maxValue;
        this.step = 1;
        this.type = 0;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        //Methods
        this.density = function(x){
            var sum = 0;
            for (var n = x; n <= this.maxValue; n = n + this.dist.step) sum = sum + this.dist.density(n) * binomial(n, x) * Math.pow(this.prob, x) * Math.pow(1 - this.prob, n - x);
            return sum;
        }
            
        this.mean = function(){
            return this.dist.mean() * this.prob;
        }
        
        this.variance = function(){
            return this.dist.mean() * this.prob * (1 - this.prob) + this.prob * this.prob * this.dist.variance();
        }
            
        this.simulate = function(){
            var trials = Math.round(this.dist.simulate());
            var successes = 0;
            for (var i = 0; i <= trials; i++) if (Math.random() <= this.prob) successes++;
            this.setValue(successes);
            return successes;
        }
    }
    _binomialN.prototype = new distribution;

    //A generic discrete distribution
    var _discrete = function (a, b, s, p){
        //Properties
        this.prob = p;
        this.minValue = a;
        this.maxValue = b;
        this.step = s;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        this.type = 0;
        
        //Methods
        this.density = function(x){
            if (x > this.maxValue || x < this.minValue) return 0;
            else{
                var k = Math.round((x - this.minValue) / this.step);
                return this.prob[k];
            }
        }
                
        this.simulate = function(){
            var p = Math.random(), sum = 0, y;
            for (var x = this.minValue; x <= this.maxValue; x++){
                if ((sum < p) && (p <= sum + this.density(x))) y = x;
                sum = sum + this.density(x);
            }
            this.setValue(y);
            return y;
        }
    }
    _discrete.prototype = new distribution;

    //Negative binomial distribution
    var _negativeBinomial = function (k, p){
        var mean = k / p, variance = k * (1 - p) / (p * p);
        var mode = Math.floor((k - 1) / p + 1);
        this.prob = p;
        this.successes = k;
        this.minValue = k;
        this.maxValue = mean + 4 * Math.sqrt(variance);
        this.step = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        this.type = 0;
        
        this.mode = function(){
            return mode;
        }
        
        this.maxDensity = function(){
            return this.density(mode);
        }
        
        this.density = function(x){
            var n = Math.round(x);
            return binomial(n - 1, k - 1) * Math.pow(p, k) * Math.pow(1 - p, n - k);
        }
            
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
            
        this.simulate = function(){
            var count = 0, trials = 0;
            while (count < successes){
                if (Math.random() < prob) count++;
                trials++;
            }
            this.setValue(trials);
            return trials;
        }   
    }
    _negativeBinomial.prototype = new distribution;

    //Normal distribution
    var _normal = function (mu, sigma){
        this.mu = mu;
        this.sigma = sigma;
        this.minValue = mu - 4 * sigma;
        this.maxValue = mu + 4 * sigma;
        this.step = (this.maxValue - this.minValue) / 100;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        this.type = 1;
        var c = 1 / (sigma * Math.sqrt(2 * Math.PI));
        
        this.mode = function(){
            return mu;
        }
        
        this.maxDensity = function(){
            return c;
        }
        
        this.density = function(x){
            var z = (x - mu) / sigma;
            return c * Math.exp(-z * z / 2);
        }
        
        this.CDF = function(x){
            var z = (x - mu) / sigma;
            if (z >= 0) return 0.5 + 0.5 * gammaCDF(0.5 * z * z, 0.5);
            else return 0.5 - 0.5 * gammaCDF(0.5 * z * z, 0.5);
        }   
        
        this.simulate = function(){
            var r = Math.sqrt(-2 * Math.log(Math.random()));
            var theta = 2 * Math.PI * Math.random();
            var x = mu + sigma * r * Math.cos(theta);
            this.setValue(x); 
            return x;
        }
            
        this.mean = function(){
            return mu;
        }
        
        this.variance = function(){
            return sigma * sigma;
        }
        
        this.stdDev = function(){
            return sigma;
        }
    }
    _normal.prototype = new distribution;

    var _gamma = function (shape, scale){
        this.shape = shape;
        this.scale = scale;
        if (shape >= 1) this.minValue = 0; else this.minValue = 0.01;
        var mean = shape * scale;
        var variance = shape * scale * scale;
        var stdDev = Math.sqrt(shape) * scale;
        var c = 1 / (gamma(shape) * Math.pow(scale, shape));
        this.maxValue = mean + 4 * stdDev;
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.mode = function(){
            if (shape < 1) return this.minValue;
            else return scale * (shape - 1);
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.density = function(x){
            return c * Math.pow(x, shape - 1) * Math.exp(-x / scale);
        }
        
        this.CDF = function(x){
            return gammaCDF(x / scale, shape);
        }
            
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
        
        this.stdDev = function(){
            return stdDev;
        }
    }
    _gamma.prototype = new distribution; 

    var _chiSquare = function (df){
        this.df = df;
        if (df == 1) this.minValue = 0.1;
        else this.minValue = 0;
        var c = 1 / (Math.pow(2, df / 2) * gamma(df / 2));
        this.maxValue = df + 4 * Math.sqrt(2 * df);
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.mode = function(){
            if (df < 2) return this.minValue;
            else return df - 2;
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.density = function(x){
            return c * Math.pow(x, df / 2 - 1) * Math.exp(-x / 2);
        }

        this.CDF = function(x){
            return gammaCDF(x / 2, df / 2);
        }
                
        this.mean = function(){
            return df;
        }
        
        this.variance = function(){
            return 2 * df;
        }
                
        this.simulate = function(){
            var V, Z, r, theta;
            V = 0;
            for (var i = 1; i <= df; i++){
                r = Math.sqrt(-2 * Math.log(Math.random()));
                theta = 2 * Math.PI * Math.random();
                Z = r * Math.cos(theta);
                V = V + Z * Z;
            }
            this.setValue(V);
            return V;
        }
    }
    _chiSquare.prototype = new distribution;

    function _student(df){
        this.df = df;
        var c = gamma((df + 1) / 2) / (Math.sqrt(df * Math.PI) * gamma(df / 2));
        if (df == 1){
            this.maxValue = 8;
            this.minValue = -8;
        }
        else if (df == 2){
            this.maxValue = 7;
            this.minValue = -7;
        }
        else{
            this.maxValue = 4 * Math.sqrt(df / (df - 2));
            this.minValue = -this.maxValue;
        }
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.mode = function(){
            return 0;
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.density = function(x){
            return c * Math.pow(1 + x * x / df, -(df + 1) / 2);
        }

        this.CDF = function(x){
            var u = df / (df + x * x);
            if (x > 0) return 1 - 0.5 * betaCDF(u, 0.5 * df, 0.5);
            else return 0.5 * betaCDF(u, 0.5 * df, 0.5);
        }
            
        this.mean = function(){
            if (df == 1) return Number.NaN;
            else return 0;
        }
        
        this.variance = function(){
            if (df == 1) return Number.NaN;
            else if (df == 2) return Infinity;
            else return df / (df - 2);
        }
                
        this.simulate = function(){
            var x, v, z, r, theta;
            v = 0;
            for (var i = 1; i <= df; i++){
                r = Math.sqrt(-2 * Math.log(Math.random()));
                theta = 2 * Math.PI * Math.random();
                z = r * Math.cos(theta);
                v = v + z * z;
            }
            r = Math.sqrt(-2 * Math.log(Math.random()));
            theta = 2 * Math.PI * Math.random();
            z = r * Math.cos(theta);
            x = z / Math.sqrt(v / df);
            this.setValue(x);
            return x;
        }
    }
    _student.prototype = new distribution;

    var _f = function (n, d){
        this.n = n; this.d = d;
        var c = (gamma((n + d) / 2) / (gamma(n / 2) * gamma(d / 2))) * Math.pow(n / d, n / 2);
        if (d == 1) this.minValue = 0.1; else this.minValue = 0;
        if (d <= 4) this.maxValue = 20; else this.maxValue = d / (d - 2)  + 4 * Math.sqrt(2.0 * (d / (d - 2)) * (d / (d - 2))   * (d + n - 2) / (n * (d - 4)));
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.mode = function(){
            if (n <= 2) return this.minValue;
            else return ((n - 2) * d) / (n * (d + 2));
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.density = function(x){
            return c * Math.pow(x, (n - 2) / 2) / Math.pow(1 + (n / d) * x, (n + d) / 2);

        }

        this.CDF = function(x){
            var u = d / (d + n * x);
            if (x < 0) return 0;
            else return 1 - betaCDF(u, 0.5 * d, 0.5 * n);
        }
            
        this.mean = function(){
            if (d <= 2) return Infinity;
            else return d / (d - 2);
        }
        
        this.variance = function(){
            if (d <= 2) return Number.NaN;
            else if (d <= 4) return Infinity;
            else return 2.0 * (d / (d - 2)) * (d / (d - 2)) * (d + n - 2) / (n * (d - 4));
        }
            
        this.simulate = function(){
            var x, U, V, Z, r, theta;
            U = 0;
            for (var i = 1; i <= n; i++){
                r = Math.sqrt(-2 * Math.log(Math.random()));
                theta = 2 * Math.PI * Math.random();
                Z = r * Math.cos(theta);
                U = U + Z * Z;
            }
            V = 0;
            for (var j = 1; j <= d; j++){
                r = Math.sqrt(-2 * Math.log(Math.random()));
                theta = 2 * Math.PI * Math.random();
                Z = r * Math.cos(theta);
                V = V + Z * Z;
            }
            x = (U / n) / (V / d);
            this.setValue(x);
            return x;
        }   
    }
    _f.prototype = new distribution;

    var _beta = function (a, b){
        this.left = a;
        this.right = b;
        var c = gamma(a + b) / (gamma(a) * gamma(b));
        if (a < 1) this.minValue = 0.01; else this.minValue = 0;
        if (b < 1) this.maxValue = 0.99; else this.maxValue = 1;
        this.step = 0.01;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.mode = function(){
            var m;
            if (a < 1 && b < 1){
                if (a < b) m = 0.01; else m = 0.99;
            }
            else if (a < 1 && b >= 1) m = 0.01;
            else if (a >= 1 && b < 1) m = 0.99;
            else m = (a - 1) / (a + b - 2);
            return m;
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.density = function(x){
            return c * Math.pow(x, a - 1) * Math.pow(1 - x, b - 1);
        }

        this.CDF = function(x){
            var bt;
            if ((x == 0) || (x == 1)) bt = 0;
            else bt = Math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * Math.log(x) + b * Math.log(1 - x));
            if (x < (a + 1) / (a + b + 2)) return bt * betaCF(x, a, b) / a;
            else return 1 - bt * betaCF(1 - x, b, a) / b;
        }
                
        this.mean = function(){
            return a / (a + b);
        }
        
        this.variance = function(){
            return a * b / ((a + b) * (a + b) * (a + b + 1));
        }
    }
    _beta.prototype = new distribution;

    var _weibull = function (k, b){
        var c = k / Math.pow(b, k);
        var mean = b * gamma(1 + 1 / k);
        var variance = b * b * gamma(1 + 2 / k) - mean * mean;
        this.minValue = 0;
        this.maxValue = mean + 4 * Math.sqrt(variance);
        this.step = this.maxValue / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);

        this.density = function(x){
            return c * Math.pow(x, k - 1) * Math.exp(-Math.pow(x / b, k));
        }
        
        this.maxDensity = function(x){
            var mode;
            if (k < 1) mode = this.minValue;
            else mode = b * Math.pow((k - 1) / k, 1 / k);
            return this.density(mode);
        }
        
        this.CDF = function(x){
            return 1 - Math.exp(-Math.pow(x / b, k));
        }
        
        this.quantile = function(p){
            return b * Math.pow(-Math.log(1 - p), 1 / k);
        }
            
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
    }
    _weibull.prototype = new distribution;

    var _pareto = function (k, b){
        var c = k * Math.pow(b, k);
        this.minValue = b;
        this.maxValue = b * (1 + 6 / k);
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);

        this.density = function(x){
            return c / Math.pow(x, k + 1);
        }
        
        this.maxDensity = function(x){
            return this.density(b);
        }
        
        this.CDF = function(x){
            return 1 - Math.pow(b / x, k);
        }
        
        this.quantile = function(p){
            return b / Math.pow((1 - p), 1 / k);
        }
            
        this.mean = function(){
            if (k <= 1) return Infinity;
            else return (k * b) / (k - 1);
        }
        
        this.variance = function(){
            if (k <= 1) return Number.NaN;
            else if (k > 1 && k <= 2) return Infinity;
            else return (k * b * b) / ((k - 1) * (k - 2) * (k - 2));
        }
    }
    _pareto.prototype = new distribution;


    var _logistic = function (a, b){
        var mean = a, variance = (b * b * Math.PI * Math.PI) / 3, stdDev = Math.sqrt(variance);
        this.minValue = mean - 4 * stdDev; 
        this.maxValue = mean + 4 * stdDev;
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.density = function(x){
            var e = Math.exp((x - a) / b);
            return e / (b * (1 + e) * (1 + e));
        }
        
        this.maxDensity = function(){
            return 1 / (4 * b);
        }
        
        this.CDF = function(x){
            var e = Math.exp((x - a) / b);
            return e / (1 + e);
        }
        
        this.quantile = function(p){
            return a + b * Math.log(p / (1 - p));
        }
        
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
    }
    _logistic.prototype = new distribution;

    var _logNormal = function (m, s){
        var mean = Math.exp(m + s * s / 2);
        var variance = Math.exp(2 * (m + s * s)) - Math.exp(2 * m + s * s);
        this.minValue = 0;
        this.maxValue = mean + 4 * Math.sqrt(variance);
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.density = function(x){
            if (x == 0) return 0; 
            else return Math.exp(-(Math.log(x) - m) * (Math.log(x) - m) / (2 * s * s)) / (Math.sqrt(2 * Math.PI) * s * x);
        }
        
        this.maxDensity = function(){
            var mode = Math.exp(m - s * s);
            return this.density(mode);
        }
        
        this.CDF = function(x){
            var z = (Math.log(x) - m) / s;
            if (z >= 0) return 0.5 + 0.5 * gammaCDF(0.5 * z * z, 0.5);
            else return 0.5 - 0.5 * gammaCDF(0.5 * z * z, 0.5);
        }
        
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
            
        this.simulate = function(){
            var r = Math.sqrt(-2 * Math.log(Math.random()));
            var theta = 2 * Math.PI * Math.random();
            var x = Math.exp(m + s * r * Math.cos(theta));
            this.setValue(x); 
            return x;
        }   
    }
    _logNormal.prototype = new distribution;
    
    var _extremeValue = function (a, b){
        var g = 0.5772156649;
        var mean = a + b * g;
        var variance = (b * b * Math.PI * Math.PI) / 6;
        var stdDev = Math.sqrt(variance);
        this.minValue = mean - 4 * stdDev;
        this.maxValue = mean + 4 * stdDev;
        this.step = (this.maxValue - this.minValue) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.density = function(x){
            var e = Math.exp(-(x - a) / b);
            return e * Math.exp(-e) / b;
        }
        
        this.maxDensity = function(){
            return this.density(a);
        }
        
        this.CDF = function(x){
            return Math.exp(-Math.exp(-(x - a) / b));
        }
        
        this.quantile = function(p){
            return a - b * Math.log(-Math.log(p));
        }
        
        this.mean = function(){
            return mean;
        }
        
        this.variance = function(){
            return variance;
        }
    }
    _extremeValue.prototype = new distribution;

    var _poisson = function (r){
        this.minValue = 0;
        this.maxValue = r + 4 * Math.sqrt(r);
        this.step = 1;
        this.type = 0;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.density = function(x){
            return Math.exp(-r) * Math.pow(r, x) / factorial(x);
        }
        
        this.mode = function(){
            return Math.floor(r);
        }
        
        this.maxDensity = function(){
            return this.density(this.mode());
        }
        
        this.CDF = function(x){
            return 1 - gammaCDF(r, x + 1);
        }
            
        this.mean = function(){
            return r;
        }
        
        this.variance = function(){
            return r;
        }
                
        this.simulate = function(){
            var arrivals = 0;
            var sum = -Math.log(1 - Math.random());
            while (sum <= r){
                arrivals++;
                sum = sum - Math.log(1 - Math.random());
            }
            this.setValue(arrivals);
            return arrivals;
        }   
    }
    _poisson.prototype = new distribution;

    var _uniform = function (a, b){
        this.minValue = a;
        this.maxValue = b;
        this.step = (b - a) / 100;
        this.type = 1;
        this.data = new Data(this.minValue, this.maxValue, this.step);
        
        this.density = function(x){
            return 1 / (b - a);
        }
            
        this.maxDensity = function(){
            return 1 / (b - a);
        }
        
        this.CDF = function(x){
            return (x - a) / (b - a);
        }
        
        this.quantile = function(p){
            return a + p * (b - a);
        }
            
        this.mean = function(){
            return (a + b) / 2;
        }
        
        this.variance = function(){
            return (b - a) * (b - a) / 12;
        }
                
        this.simulate = function(){
            var x = a + b * Math.random();
            this.setValue(x);
            return x;
        }   
    }   
    _uniform.prototype = new distribution;    

    return {

        binomial:_binomial,
        locationScale: _locationScale,
        convolution = _convolution,
        orderStatistic = _orderStatistic,
        binomialN = _binomialN,
        discrete = _discrete,
        negativeBinomial = _negativeBinomial,
        normal = _normal,
        gamma = _gamma,
        chiSquare = _chiSquare,
        student = _student,
        f = _f,
        beta = _beta,
        weibull = _weibull,
        pareto = _pareto,
        logistic = _logistic,
        logNormal = _logNormal,
        extremeValue = _extremeValue,
        poisson = _poisson,
        uniform = _uniform

    }



})(window);




b = new socr.distribution.binomial(1,2);
