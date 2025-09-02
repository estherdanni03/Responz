# 🚑 Responz - On-Chain Ambulance Dispatch System

A decentralized emergency response coordination system built on the Stacks blockchain using Clarity smart contracts.

## 📋 Overview

Responz enables transparent, immutable emergency response coordination by connecting emergency callers with available ambulances through blockchain technology. The system ensures accountability, tracks response times, and maintains a permanent record of all emergency interactions.

## ✨ Features

- 🆘 **Emergency Registration**: Citizens can register emergencies with location and severity
- 🚑 **Ambulance Management**: Ambulance operators can register and manage their vehicles
- 📡 **Smart Dispatch**: Authorized dispatchers can assign ambulances to emergencies
- 📍 **Real-time Tracking**: Location updates and status tracking for all parties
- 💰 **Payment Integration**: Built-in fee system for emergency services
- 📊 **Analytics**: Comprehensive statistics and emergency history
- 🔐 **Authorization System**: Role-based access control for dispatchers

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing
- Basic understanding of Clarity smart contracts

### Installation

```bash
clarinet new responz-project
cd responz-project
```

Copy the contract code into `contracts/Responz.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage

### For Emergency Callers

#### Register an Emergency
```clarity
(contract-call? .Responz register-emergency latitude longitude severity-level)
```

Example:
```clarity
(contract-call? .Responz register-emergency 40742054 -73989308 u3)
```

#### Check Emergency Status
```clarity
(contract-call? .Responz get-emergency emergency-id)
```

### For Ambulance Operators

#### Register Ambulance
```clarity
(contract-call? .Responz register-ambulance latitude longitude)
```

#### Update Location
```clarity
(contract-call? .Responz update-ambulance-location ambulance-id new-lat new-lng)
```

#### Update Status
```clarity
(contract-call? .Responz set-ambulance-status ambulance-id status)
```

### For Dispatchers

#### Dispatch Ambulance
```clarity
(contract-call? .Responz dispatch-ambulance emergency-id ambulance-id)
```

#### Update Emergency Status
```clarity
(contract-call? .Responz update-emergency-status emergency-id new-status)
```

## 📊 Status Codes

### Emergency Status
- `0` - Pending
- `1` - Dispatched  
- `2` - En Route
- `3` - On Scene
- `4` - Completed
- `5` - Cancelled

### Ambulance Status
- `0` - Available
- `1` - Busy
- `2` - Maintenance

## 💡 Key Functions

| Function | Description | Access |
|----------|-------------|---------|
| `register-emergency` | Create new emergency request | Public |
| `register-ambulance` | Register ambulance service | Public |
| `dispatch-ambulance` | Assign ambulance to emergency | Authorized |
| `update-emergency-status` | Update emergency progress | Caller/Authorized |
| `get-emergency` | Retrieve emergency details | Read-only |
| `get-stats` | System statistics | Read-only |

## 🔧 Configuration

### Base Fee
The system charges a base fee of 1 STX (1,000,000 microSTX) for emergency registration.

### Authorization
Contract owner can authorize dispatchers using:
```clarity
(contract-call? .Responz authorize-dispatcher dispatcher-principal)
```

## 🛡️ Security Features

- Payment verification before emergency registration
- Role-based access control
- Status validation
- Ambulance availability checks
- Immutable emergency history

## 📈 Analytics

The contract provides comprehensive statistics including:
- Total emergencies registered
- Completed responses
- Active emergencies
- Registered ambulances
- Individual ambulance response counts

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Test your changes with Clarinet
4. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

## 🆘 Support

For technical support or questions about the Responz system, please open an issue in the repository.


