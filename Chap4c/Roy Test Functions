#FUNCTIONS
#########################################################################

getParam = function(x) {
    Param = list(
        meanX=x$coefficients$fixed[1],
        meanY=x$coefficients$fixed[2],
        DVarX=as.numeric(VarCorr(MCS1)[1,1]),
        DVarY=as.numeric(VarCorr(MCS1)[2,1]),
        DCovXY=sqrt(DVarX*DVarY),
        SigVarX=x$sigma,
        SigVarY=x$sigma*max(intervals(x)$varStruct[2],is.null(intervals(x)$varStruct[2])),
        SigCovXY=x$sigma*max(intervals(x)$corStruct[2],is.null(intervals(x)$corStruct[2])-1)
        )
    Param
    }

#########################################################################


getD = function(x) {
    v = as.numeric(VarCorr(x)[1:2,1])
    r = as.numeric(VarCorr(x)[2,3])
    Dhat = matrix(0,2,2)
    diag(Dhat) = v
    Dhat[1,2] = Dhat[2,1] = r * sqrt(prod(v))
    arr=rownames(summary(x)$contrasts$method)
    colnames(Dhat)=arr
    rownames(Dhat)=arr
    Dhat
}
#########################################################################


getSigma = function(x) {
    res = as.numeric(VarCorr(x)[6])
    V = max(is.null(intervals(x)$varStruct[2]),intervals(x)$varStruct[2])
    C = intervals(x)$corStruct[2]
    Sighat = matrix(1,2,2)
    Sighat[2,2] = as.numeric(VarCorr(x)[3,1])
    Sighat[1,1] = as.numeric(VarCorr(x)[3,1])*(V^2)
    Sighat[1,2] = Sighat[2,1] = as.numeric(VarCorr(x)[3,1])*V*C
    arr=rownames(summary(x)$contrasts$method)
    colnames(Sighat)=arr
    rownames(Sighat)=arr
    Sighat
}

#########################################################################
getOmega = function(x) {
    Omega = matrix(1,2,2)
    Omega[1,1] = getD(x)[1,1] + getSigma(x)[1,1] 
    Omega[2,2] = getD(x)[2,2] + getSigma(x)[2,2]
    Omega[1,2] = Omega[2,1] = getD(x)[1,2] + getSigma(x)[1,2]
    Omega
}
#########################################################################

# Get Coefficient of Repeatability

getCR = function(x) {
    CR = numeric(2)
    V = max(is.null(intervals(x)$varStruct[2]),intervals(x)$varStruct[2])
    CR[1] = 1.96 * sqrt(2) * as.numeric(VarCorr(x)[6])
    CR[2] = 1.96 * sqrt(2) * as.numeric(VarCorr(x)[6]) * V
    CR
}

#########################################################################

# Get Correlation Matrix

getCorMat= function(x) {
    CorMat = matrix(1,4,4)
    Rho.xy = getOmega2(x)[2,1]/ sqrt(getOmega2(x)[1,1]*getOmega2(x)[2,2])
    CorMat[1,2] = CorMat[2,1] = CorMat[3,4] = CorMat[4,3] = Rho.xy

    CorMat[1,3] = CorMat[3,1] =  getD(x)[1,1]/getOmega2(x)[1,1]
    CorMat[4,2] = CorMat[2,4] =  getD(x)[2,2]/getOmega2(x)[2,2]
    CorMat[4,1] = CorMat[3,2] = CorMat[2,3] = CorMat[1,4] = Rho.xy*(getD(x)[1,2]/getOmega2(x)[1,2])
    CorMat
}


#########################################################################
