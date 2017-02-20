import math
class Sample(object):

# outward facing methods
    def __init__(self, n=None):
        functionName = "Sample.__init__: "
        if(n == None):
            raise ValueError(functionName + "invalid n")
        if(not(isinstance(n, int))):
            raise ValueError(functionName + "invalid n")
        if((n < 2) or (n >= 30)):
            raise ValueError(functionName + "invalid n")
        self.n = n

    def getN(self):
        return self.n

    
    def p(self, t=None, tails=1):
        functionName = "Sample.p: "
        if(t == None):
            raise ValueError(functionName + "missing t")
        if(not(isinstance(t, float))):
            raise ValueError(functionName + "invalid t")
        if(t < 0.0):
            raise ValueError(functionName + "invalid t")
        
        if(not(isinstance(tails, int))):
            raise ValueError(functionName + "invalid tails")
        if((tails != 1) & (tails != 2)):
            raise ValueError(functionName + "invalid tails")
        
        constant = self.calculateConstant(self.n)
        integration = self.integrate(t, self.n, self.f)
        if(tails == 1):
            result = constant * integration + 0.5
        else:
            result = constant * integration * 2
            
        if(result > 1.0):
            raise ValueError(functionName + "result > 1.0")
        
        return result
        
# internal methods
    def gamma(self, x):
        if(x == 1):
            return 1
        if(x == 0.5):
            return math.sqrt(math.pi)
        return (x - 1) * self.gamma(x - 1)
    
    def calculateConstant(self, n):
        n = float(n)
        numerator = self.gamma((n + 1.0) / 2.0)
        denominator = self.gamma(n / 2.0) * math.sqrt(n * math.pi)
        result = numerator / denominator
        return result
    
    def f(self, u, n):
        n = float(n)
        base = (1 + (u ** 2) / n)
        exponent = -(n + 1.0) / 2
        result = base ** exponent
        return result
    
    def integrate(self, t , n , f):
        lowBound = 0
        highBound = t
        # Simpson integration part
        epsilon = 0.001
        simpsonOld = 0.0
        simpsonNew = epsilon
        s = 4
        while abs((simpsonNew-simpsonOld)/simpsonNew) > epsilon:
            simpsonOld = simpsonNew
            w = (highBound - lowBound) / s
            oddSum = 0
            evenSum = 0
            i = 0
            t1 = lowBound + 2 * w
            t2 = lowBound + w
            while i != highBound:
                oddSum += f(t1, n)
                i = i + 2 * w
                t1 = t1 + 2 * w
            while i != highBound + w:
                evenSum += f(t2, n)
                i = i + 2 * w
                t2 = t + 2 * w
            he = f(lowBound, n) + f(highBound, n) + 2 * oddSum + 4 * evenSum
            simpsonNew = (w / 3) * (he)
            s = s * 2
        return simpsonNew

