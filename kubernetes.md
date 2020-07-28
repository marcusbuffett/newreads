# Kubernetes commands

Someo of the various kubernetes-related commands I had to run to set up the
cluster

Installed metrics monitoring stack and linkerd stack, 1-click apps

Install kong

```
helm install kong/kong --generate-name --set ingressController.installCRDs=false
```

Apply manifests:

```
TODO
```

start bash:

```
kubectl run -it --rm --image=ubuntu --restart=Never bash -- /bin/bash
```

Copy psql:

```
k cp dump.sql.gz bash:/
```

cert manager:

```
kubectl apply --validate=false -f https://github.com/jetstack/cert-manager/releases/download/v0.16.0/cert-manager.yaml
```

ackee + mongo:

```
helm install ackee ackee/ackee-chart --values ackee-values.yml
helm repo add bitnami https://charts.bitnami.com/bitnami
helm install ackee-mongo bitnami/mongodb
```

ackee connect:

```
export POD_NAME=$(kubectl get pods --namespace default -l "app=ackee-chart,release=ackee" -o jsonpath="{.items[0].metadata.name}")                                                                                      NORMAL
echo "Visit http://127.0.0.1:8080 to use your application"
kubectl port-forward $POD_NAME 8082:80
```
